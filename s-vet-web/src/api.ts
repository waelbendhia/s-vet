import axios from "axios";
import { DateObject, DateTime } from "luxon";
import {
  ConsultationSearchRequest,
  SearchResult,
  FullConsultation,
  ConsultationStatistics,
  Owned,
  Keyed,
  Pet,
  WithPet,
  Consultation,
  Owner,
  FullWithPrevious,
  Act,
  FullOwner,
  FullPet,
  Settings,
  timeOfDayFromString,
  timeOfDayToString,
  WeekDay,
  WorkHours,
  mapPartialRecord,
  ConsultationsByHour,
} from "./types";

type ChangePropType<Object, Property, Type> = {
  [K in keyof Object]: K extends Property ? Type : Object[K];
};

const consultationTimeToLuxon = <C extends Consultation>({
  time,
  ...c
}: ChangePropType<C, "time", string>): C =>
  // @ts-ignore
  ({ ...c, time: DateTime.fromISO(time).toObject() });

const dateObjectToIso = (d?: DateObject): string | undefined =>
  !!d ? DateTime.fromObject(d).toISO() : undefined;

const dateObjectToIsoDate = (d?: DateObject): string | undefined =>
  !!d ? DateTime.fromObject(d).toISODate() : undefined;

export const searchConsultations = ({
  before,
  after,
  ...params
}: ConsultationSearchRequest): Promise<SearchResult<FullConsultation>> =>
  axios
    .get<SearchResult<ChangePropType<FullConsultation, "time", string>>>(
      "/api/consultations",
      {
        params: {
          ...params,
          before: dateObjectToIso(before),
          after: dateObjectToIso(after),
        },
      },
    )
    .then((d) => ({
      ...d.data,
      rows: d.data.rows.map(consultationTimeToLuxon),
    }));

export const getConsultationStatistics = () =>
  axios
    .get<ConsultationStatistics>("/api/statistics/consultation-totals")
    .then((x) => x.data);

export const getConsultationsByHour = (params: {
  after?: string;
  before?: string;
}) =>
  axios
    .get<ConsultationsByHour>("/api/statistics/consultations-by-hour", {
      params,
    })
    .then((x) => x.data);

export type SearchRequest = {
  page?: number;
  itemsPerPage?: number;
  search?: string;
};

const petAgeToLuxon = <P extends Pet>({
  age,
  ...p
}: ChangePropType<P, "age", string>): P =>
  // @ts-ignore
  ({ ...p, age: DateTime.fromISO(age).toObject() });

export const searchPets = ({
  page = 0,
  itemsPerPage = 10,
  ...params
}: SearchRequest): Promise<SearchResult<Owned<Keyed<Pet>>>> =>
  axios
    .get<SearchResult<ChangePropType<Owned<Keyed<Pet>>, "age", string>>>(
      "/api/pets",
      { params: { page, itemsPerPage, ...params } },
    )
    .then((r) => ({ ...r.data, rows: r.data.rows.map(petAgeToLuxon) }));

type APIFullPet = ChangePropType<
  ChangePropType<FullPet, "age", string>,
  "consultations",
  ChangePropType<Keyed<Consultation>, "time", string>[]
>;

export const getPet = (pKey: number): Promise<FullPet> =>
  axios.get<APIFullPet>(`/api/pets/${pKey}`).then((d) => {
    const p = petAgeToLuxon<
      ChangePropType<
        FullPet,
        "consultations",
        ChangePropType<Keyed<Consultation>, "time", string>[]
      >
    >(d.data);
    return {
      ...p,
      consultations: p.consultations.map(consultationTimeToLuxon),
    };
  });

export const updatePet = ({ key, age, ...p }: Keyed<Pet>) =>
  axios
    .put<ChangePropType<Keyed<Pet>, "age", string>>(`/api/pets/${key}`, {
      ...p,
      age: dateObjectToIsoDate(age),
    })
    .then((d) => petAgeToLuxon(d.data));

export const createConsultation = (c: WithPet<Consultation>) =>
  axios.post<Keyed<Consultation>>(`/api/pets/${c.pet.key}/consultations`, {
    ...c,
    pet: null,
  });

export const updateConsultation = ({ key, ...c }: Keyed<Consultation>) =>
  axios
    .put<ChangePropType<Keyed<Consultation>, "time", string>>(
      `/api/consultations/${key}`,
      c,
    )
    .then((d) => consultationTimeToLuxon(d.data));

export const createPet = ({ age, ...p }: Owned<Pet>) =>
  axios
    .post<ChangePropType<Owned<Keyed<Pet>>, "age", string>>(
      `/api/owners/${p.owner.key}/pets`,
      {
        ...p,
        age: dateObjectToIsoDate(age),
        owner: null,
      },
    )
    .then((d) => ({ ...petAgeToLuxon(d.data), owner: p.owner }));

export const searchOwners = ({
  page = 0,
  itemsPerPage = 10,
  ...params
}: SearchRequest): Promise<SearchResult<Keyed<Owner>>> =>
  axios
    .get("/api/owners", { params: { page, itemsPerPage, ...params } })
    .then((r) => r.data);

export const createOwner = (o: Owner) =>
  axios.post<Keyed<Owner>>("/api/owners", o);

export const updateOwner = ({ key, ...o }: Keyed<Owner>) =>
  axios.put<Keyed<Owner>>(`/api/owners/${key}`, o).then((d) => d.data);

export const cancelConsultation = (cKey: number) =>
  axios.delete(`/api/consultations/${cKey}`);

type APIFullWithPrevious = ChangePropType<
  ChangePropType<
    ChangePropType<FullWithPrevious, "time", string>,
    "previous",
    ChangePropType<Keyed<Consultation>, "time", string>[]
  >,
  "pet",
  ChangePropType<Owned<Keyed<Pet>>, "age", string>
>;

export const getConsultation = (cKey: number): Promise<FullWithPrevious> =>
  axios.get<APIFullWithPrevious>(`/api/consultations/${cKey}`).then((d) => {
    const c = consultationTimeToLuxon<
      ChangePropType<APIFullWithPrevious, "time", DateObject>
    >(d.data);

    return {
      ...c,
      pet: petAgeToLuxon(c.pet),
      previous: c.previous.map(consultationTimeToLuxon),
    };
  });

export const getActs = (params: { search?: string }) =>
  axios.get<Keyed<Act>[]>(`/api/acts`, { params }).then((d) => d.data);

export const deleteAct = (aKey: number) =>
  axios.delete<void>(`/api/acts/${aKey}`);

export const createAct = (act: Act) => axios.post<Keyed<Act>>(`/api/acts`, act);
export const updateAct = ({ key, ...act }: Keyed<Act>) =>
  axios.put<Keyed<Act>>(`/api/acts/${key}`, act);

type WorkHoursAPI = Partial<Record<WeekDay, { start: string; end: string }>>;

const convertWorkHoursAPIToWorkHours = (x: WorkHoursAPI): WorkHours =>
  mapPartialRecord(x, (v) => ({
    start: timeOfDayFromString(v.start) ?? { h: 9, m: 0, s: 0 },
    end: timeOfDayFromString(v.end) ?? { h: 18, m: 0, s: 0 },
  }));

export const getSettings = (): Promise<Settings> =>
  axios.get(`/api/settings`).then((d) => {
    const { schedule, ...rest } = d.data;
    return {
      schedule: convertWorkHoursAPIToWorkHours(schedule),
      ...rest,
    };
  });

const convertWorkHoursToWorkHoursAPI = (x: WorkHours): WorkHoursAPI =>
  mapPartialRecord(x, (v) => ({
    start: timeOfDayToString(v.start),
    end: timeOfDayToString(v.end),
  }));

export const updateSettings = ({ schedule, ...s }: Settings) =>
  axios.put<void>(`/api/settings`, {
    ...s,
    schedule: convertWorkHoursToWorkHoursAPI(schedule),
  });

type APIFullOwner = ChangePropType<
  FullOwner,
  "pets",
  ChangePropType<Keyed<Pet>, "age", string>[]
>;

export const getOwner = (oKey: number): Promise<FullOwner> =>
  axios.get<APIFullOwner>(`/api/owners/${oKey}`).then((d) => ({
    ...d.data,
    pets: d.data.pets.map(petAgeToLuxon),
  }));

export const updatePassword = (r: {
  oldPassword: string;
  newPassword: string;
}) => axios.post<void>("/api/change-password", r).then((d) => d.data);
