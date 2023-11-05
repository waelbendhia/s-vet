import { DateObject, DateTime } from 'luxon';
import { useEffect, useState } from 'react';

export type Keyed<T> = T & { key: number };

export type Act = { price: number; name: string };

export type AppliedTreatment = {
  quantity: number;
  treatment:
    | { tag: 'General'; contents: string }
    | { tag: 'Specific'; contents: Keyed<Act> };
};

export const treatmentName = (t: AppliedTreatment) =>
  t.treatment.tag === 'Specific'
    ? t.treatment.contents.name
    : t.treatment.contents;

export const eqAppliedTreatment = (
  a: AppliedTreatment,
  b: AppliedTreatment
): boolean =>
  (a.treatment.tag === 'Specific' &&
    b.treatment.tag === 'Specific' &&
    a.treatment.contents.key === b.treatment.contents.key) ||
  (a.treatment.tag === 'General' &&
    b.treatment.tag === 'General' &&
    a.treatment.contents === b.treatment.contents);

export type Consultation = {
  weight?: number;
  motive: string;
  diagnosis?: string;
  treatment: AppliedTreatment[];
  time: DateObject;
  amount?: {
    total: number;
    paid: number;
    remaining: number;
  };
};

export type Owner = {
  name: string;
  email?: string;
  phonenumber?: string;
  address?: string;
};

export type Pet = {
  name: string;
  age: DateObject;
  species: 'Cat' | 'Dog';
  sex: 'Male' | 'Female';
  breed?: string;
  neutered: boolean;
  bloodType?: string;
  allergies: string[];
};

export type FullPet = Owned<Keyed<Pet>> & {
  consultations: Keyed<Consultation>[];
};

export type Owned<T> = T & { owner: Keyed<Owner> };

export type FullConsultation = Keyed<Consultation> & { pet: Owned<Keyed<Pet>> };

export type FullOwner = Keyed<Owner> & {
  pets: Keyed<Pet>[];
  paid: number;
  unpaid: number;
};

export type FullWithPrevious = FullConsultation & {
  previous: Keyed<Consultation>[];
};

export type WithPet<T> = T & { pet: Keyed<Pet> };

export type SearchRequest = {
  page?: number;
  itemsPerPage?: number;
  search?: string;
};

export type ConsultationSearchRequest = SearchRequest & {
  after?: DateObject;
  before?: DateObject;
  status?: 'pending' | 'completed';
};

export type SearchResult<d> = { rows: d[]; total: number };

export type Loadable<T> = T | 'NotRequested' | 'Loading' | 'NetworkError';

export type WithMemo<T> = {
  value: Loadable<T>;
  memo: T;
};

export const isOk = <T>(v: Loadable<T>): v is T =>
  v !== 'Loading' && v !== 'NetworkError' && v !== 'NotRequested';

export const extractValue = <T>(m: WithMemo<T>) =>
  isOk(m.value) ? m.value : m.memo;

export const initMemo = <T>(empty: T): WithMemo<T> => ({
  value: 'NotRequested',
  memo: empty,
});

export const toComponentState = <T>(m: WithMemo<T>) => ({
  data: extractValue(m),
  state: isOk(m.value) ? 'Ok' : m.value,
});

export type ConsultationStatistics = {
  remainingConsultations: number;
  doneConsultations: number;
  earnings: number;
};

type SpeciesWithSex = `${Pet['species']}-${Pet['sex']}`;

export const translateSpeciesWithSex = (p: Pet): string => {
  let v: string;
  switch (`${p.species}-${p.sex}` as SpeciesWithSex) {
    case 'Dog-Male':
      v = 'Chien';
      break;
    case 'Dog-Female':
      v = 'Chienne';
      break;
    case 'Cat-Male':
      v = 'Chat';
      break;
    case 'Cat-Female':
      v = 'Chatte';
      break;
  }

  if (!!p.breed) {
    return `${v} ${p.breed}`;
  }

  return v;
};

export const tndFormat = Intl.NumberFormat('fr', {
  style: 'currency',
  currency: 'TND',
});

const frNumberFormat = new Intl.NumberFormat('fr');

export const currencyParser = (val: string) => {
  if (val.length === 0) {
    val = '0,0';
  }

  // detecting and parsing between comma and dot
  let group = frNumberFormat.format(1111).replace(/1/g, '');
  let decimal = frNumberFormat.format(1.1).replace(/1/g, '');
  let reversedVal = val
    .replace(new RegExp('\\' + group, 'g'), '')
    .replace(new RegExp('\\' + decimal, 'g'), '.')
    .replace(/[^0-9.]/g, '');

  const digitsAfterDecimalCount = (reversedVal.split(',')[1] || []).length;
  const needsDigitsAppended = digitsAfterDecimalCount > 2;

  let n: number = Number.parseFloat(reversedVal);

  if (needsDigitsAppended) {
    n = n * Math.pow(10, digitsAfterDecimalCount - 2);
  }

  return Number.isNaN(n) ? 0 : n;
};

export type PriceType = 'consultation' | 'emergency';

export type Prices = Record<PriceType, number>;

export type TimeOfDay = { h: number; m: number; s: number };

export const timeOfDayFromString = (str: string) => {
  const split = str.split(':');

  if (split.length !== 3) {
    return undefined;
  }

  const [h, m, s] = split;

  return {
    h: Number.parseInt(h),
    m: Number.parseInt(m),
    s: Number.parseInt(s),
  };
};

const fmtTimeSlot = (n: number): string => n.toFixed(0).padStart(2, '0');

export const timeOfDayToString = (t: TimeOfDay): string =>
  `${fmtTimeSlot(t.h > 23 ? 23 : t.h)}:${fmtTimeSlot(
    t.m > 59 ? 59 : t.m
  )}:${fmtTimeSlot(t.s > 59 ? 59 : t.s)}`;

export const compareDateTimeToTimeOfDay = (
  d: DateTime,
  t: TimeOfDay
): -1 | 0 | 1 => {
  if (d.hour < t.h) {
    return -1;
  }
  if (d.hour > t.h) {
    return 1;
  }

  if (d.minute < t.m) {
    return -1;
  }
  if (d.minute > t.m) {
    return 1;
  }

  if (d.second < t.s) {
    return -1;
  }
  if (d.second > t.s) {
    return 1;
  }

  return 0;
};

export type WeekDay =
  | 'monday'
  | 'tuesday'
  | 'wednesday'
  | 'thursday'
  | 'friday'
  | 'saturday'
  | 'sunday';

export const daysOfTheWeek: WeekDay[] = Object.keys({
  monday: {},
  tuesday: {},
  wednesday: {},
  thursday: {},
  friday: {},
  saturday: {},
  sunday: {},
} as Record<WeekDay, {}>) as WeekDay[];

const weekdayTranslations: Record<WeekDay, string> = {
  monday: 'lundi',
  tuesday: 'mardi',
  wednesday: 'mercredi',
  thursday: 'jeudi',
  friday: 'vendredi',
  saturday: 'samedi',
  sunday: 'dimanche',
};

export const translateWeekday = (w: WeekDay): string => weekdayTranslations[w];

export const weekDayToEnum = (w: WeekDay): 1 | 2 | 3 | 4 | 5 | 6 | 7 => {
  switch (w) {
    case 'monday':
      return 1;
    case 'tuesday':
      return 2;
    case 'wednesday':
      return 3;
    case 'thursday':
      return 4;
    case 'friday':
      return 5;
    case 'saturday':
      return 6;
    case 'sunday':
      return 7;
  }
};

export const weekDayFromEnum = (w: 1 | 2 | 3 | 4 | 5 | 6 | 7): WeekDay => {
  switch (w) {
    case 1:
      return 'monday';
    case 2:
      return 'tuesday';
    case 3:
      return 'wednesday';
    case 4:
      return 'thursday';
    case 5:
      return 'friday';
    case 6:
      return 'saturday';
    case 7:
      return 'sunday';
  }
};

export type WorkHours = Partial<
  Record<
    WeekDay,
    {
      start: TimeOfDay;
      end: TimeOfDay;
    }
  >
>;

export type Settings = {
  prices: Prices;
  schedule: WorkHours;
};

export const useDebounce = <T>(value: T, delay: number) => {
  // State and setters for debounced value
  const [debouncedValue, setDebouncedValue] = useState(value);

  useEffect(
    () => {
      // Set debouncedValue to value (passed in) after the specified delay
      const handler = setTimeout(() => {
        setDebouncedValue(value);
      }, delay);

      // Return a cleanup function that will be called every time ...
      // ... useEffect is re-called. useEffect will only be re-called ...
      // ... if value changes (see the inputs array below).
      // This is how we prevent debouncedValue from changing if value is ...
      // ... changed within the delay period. Timeout gets cleared and restarted.
      // To put it in context, if the user is typing within our app's ...
      // ... search box, we don't want the debouncedValue to update until ...
      // ... they've stopped typing for more than 500ms.
      return () => {
        clearTimeout(handler);
      };
    },
    // Only re-call effect if value changes
    // You could also add the "delay" var to inputs array if you ...
    // ... need to be able to change that dynamically.
    [value, delay]
  );

  return debouncedValue;
};

export const formatMass = (m: number): string =>
  m < 1000 ? `${m} g` : `${frNumberFormat.format(m / 1000)} kg`;

export const formatAge = (a: DateTime) => {
  const { years, months, days } = DateTime.local()
    .diff(a)
    .shiftTo('years', 'months', 'days')
    .normalize();

  let parts = [];
  if (years > 0) {
    parts.push(`${Math.trunc(years)} an${years > 1 ? 's' : ''}`);
  }

  if (months > 0) {
    parts.push(`${Math.trunc(months)} mois`);
  }

  if (days > 0) {
    parts.push(`${Math.trunc(days)} jour${days > 1 ? 's' : ''}`);
  }

  let fmt = '';

  for (let i = 0; i < parts.length; i++) {
    if (fmt.length > 0 && i === parts.length - 1) {
      fmt += ' et ' + parts[i];
    } else if (fmt.length > 0) {
      fmt += ', ' + parts[i];
    } else {
      fmt += parts[i];
    }
  }

  return fmt;
};

export const mapRecord = <K extends string | number, S, T>(
  rs: Record<K, S>,
  fa: (_: S) => T
): Record<K, T> => {
  // @ts-ignore
  let rt: Record<K, T> = {};

  for (const k in rs) {
    rt[k] = fa(rs[k]);
  }

  return rt;
};

export const mapPartialRecord = <K extends string | number, S, T>(
  rs: Partial<Record<K, S>>,
  fa: (_: S) => T
): Partial<Record<K, T>> => {
  let rt: Partial<Record<K, T>> = {};

  for (const k in rs) {
    const v = rs[k];
    if (!!v) {
      // @ts-ignore
      rt[k] = fa(v);
    }
  }

  return rt;
};

type Hour =
  | 0
  | 1
  | 2
  | 3
  | 4
  | 5
  | 6
  | 7
  | 8
  | 9
  | 10
  | 11
  | 12
  | 13
  | 14
  | 15
  | 16
  | 17
  | 18
  | 19
  | 20
  | 21
  | 22
  | 23;

export const allHours: Hour[] = [
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12,
  13,
  14,
  15,
  16,
  17,
  18,
  19,
  20,
  21,
  22,
  23,
];

export type DayHour = `${WeekDay}-${Hour}`;

export type ConsultationsByHour = Partial<
  Record<DayHour, { numberOfConsultations: number; revenue: number }>
>;

export const allDaysHours: DayHour[] = daysOfTheWeek.flatMap((d) =>
  allHours.map((h) => `${d}-${h}` as DayHour)
);
