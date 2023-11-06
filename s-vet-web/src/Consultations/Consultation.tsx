import {
  Form,
  Input,
  DatePicker,
  Spin,
  InputNumber,
  Button,
  Divider,
} from "antd";
import { DateObject, DateTime } from "luxon";
import moment from "moment";
import { useEffect, useState } from "react";
import { useParams } from "react-router";
import {
  isOk,
  Keyed,
  Consultation,
  extractValue,
  Owner,
  Pet,
  Settings,
  compareDateTimeToTimeOfDay,
  weekDayFromEnum,
} from "../types";
import { selectConsultationAction, updateConsultationAction } from "./state";
import { DescriptionsItem } from "../Descriptions";
import ActSelect from "../Acts/ActSelect";
import Previous from "./Previous";
import OwnerPreview from "../Owners/OwnerPreview";
import PetPreview from "../Pets/PetPreview";
import FocusView from "../FocusView";
import Checkbox from "antd/lib/checkbox/Checkbox";
import Price from "../Price";
import { useAppDispatch, useAppSelector } from "../hooks";

const checkWeekDay = (n: number): n is 1 | 2 | 3 | 4 | 5 | 6 | 7 =>
  n === 1 || n === 2 || n === 3 || n === 4 || n === 5 || n === 6 || n === 7;

const calculateBasePrice = ({ prices, schedule }: Settings): number => {
  const now = DateTime.local();

  if (!checkWeekDay(now.weekday)) {
    return prices.emergency;
  }

  const dow = weekDayFromEnum(now.weekday);
  const times = schedule[dow];

  if (!times) {
    return prices.emergency;
  }

  const { start, end } = times;

  if (compareDateTimeToTimeOfDay(now, start) === -1) {
    return prices.emergency;
  }

  if (compareDateTimeToTimeOfDay(now, end) === 1) {
    return prices.emergency;
  }

  return prices.consultation;
};

const dateObjectToJSDate = (d?: DateObject): Date | undefined =>
  !!d ? DateTime.fromObject(d).toJSDate() : undefined;

const ConsultationView = () => {
  const { c, basePrice, state } = useAppSelector((s) => ({
    c: s.consultations.current,
    basePrice: calculateBasePrice(extractValue(s.settings.settings)),
    state: s.consultations.updateState,
  }));
  const dispatch = useAppDispatch();

  const { key } = useParams<{ key: string }>();
  const cKey = Number.parseInt(key ?? "");
  const [form] = Form.useForm<
    Keyed<Consultation> & { time: moment.Moment; withConsultation: boolean }
  >();

  const [treatmentPrice, setTreatmentPrice] = useState<number>(0);
  const [withConsultation, setWithConsultation] = useState<boolean>(true);
  const [price, setPrice] = useState<number>(basePrice);
  const [totalPrice, setTotalPrice] = useState<number>(0);
  const [previous, setPrevious] = useState<Keyed<Consultation>[]>([]);
  const [owner, setOwner] = useState<Keyed<Owner>>({ key: 0, name: "" });
  const [pet, setPet] = useState<Keyed<Pet>>({
    key: 0,
    name: "",
    age: DateTime.local().toObject(),
    sex: "Male",
    species: "Dog",
    neutered: false,
    allergies: [],
  });

  useEffect(() => {
    setPrice(withConsultation ? basePrice + treatmentPrice : treatmentPrice);
  }, [basePrice, treatmentPrice, withConsultation]);

  useEffect(() => {
    if (isOk(c)) {
      setPrevious(c.previous);
      setOwner(c.pet.owner);
      setPet(c.pet);
    }
  }, [c]);

  useEffect(() => {
    if (!isNaN(cKey)) {
      dispatch(selectConsultationAction(cKey));
    }
  }, [dispatch, cKey]);

  useEffect(() => {
    if (isOk(c)) {
      form.resetFields();
    }
  }, [form, c]);

  return (
    <FocusView
      title="Consultation"
      subTitle={isOk(c) ? c.pet.name : ""}
      sideContent={
        <Spin spinning={c === "Loading"}>
          <h2>Patient</h2>
          <PetPreview pet={pet} />
          <Divider />
          <h2>Propriétaire</h2>
          <OwnerPreview owner={owner} />
          <Divider />
          <Previous previous={previous} />
        </Spin>
      }
    >
      <h2>Consultation Actuelle</h2>
      <Spin spinning={c === "Loading"}>
        <Form
          layout="vertical"
          form={form}
          initialValues={
            isOk(c)
              ? {
                  ...c,
                  time: moment(dateObjectToJSDate(c.time)),
                  withConsultation: withConsultation,
                  amount: !!c.amount
                    ? {
                        total: c.amount.total / 1000,
                        paid: c.amount.paid / 1000,
                        remaining: c.amount.remaining / 1000,
                      }
                    : undefined,
                }
              : undefined
          }
          onValuesChange={(_, c) => {
            setTreatmentPrice(
              (c.treatment ?? []).reduce(
                (p, c) =>
                  c.treatment.tag === "Specific"
                    ? p + c.treatment.contents.price
                    : p + 0,
                0,
              ),
            );
            setTotalPrice((p) => c.amount?.total ?? p);
            setWithConsultation(c.withConsultation);
          }}
          onFinish={({ withConsultation, ...v }) => {
            if (isOk(c)) {
              const total = (v.amount?.total ?? 0) * 1000;
              const paid = (v.amount?.paid ?? 0) * 1000;
              const updated: Keyed<Consultation> = {
                ...v,
                key: c.key,
                time: DateTime.fromJSDate(v.time.toDate()),
                amount: {
                  total,
                  paid,
                  remaining: total - paid,
                },
              };
              dispatch(updateConsultationAction(updated));
            }
          }}
        >
          <Form.Item name="time" label="Date" rules={[{ required: true }]}>
            <DatePicker showTime={true} />
          </Form.Item>
          <Form.Item name="motive" label="Motif" rules={[{ required: true }]}>
            <Input />
          </Form.Item>
          <Form.Item name="diagnosis" label="Diagnostic">
            <Input.TextArea autoSize={{ minRows: 3, maxRows: 3 }} />
          </Form.Item>
          <Form.Item name="treatment" label="Traitement">
            <ActSelect />
          </Form.Item>
          <Form.Item name="weight" label="Poid en grammes">
            <InputNumber />
          </Form.Item>
          <div className="form-row-flex">
            <Form.Item name="withConsultation" valuePropName="checked">
              <Checkbox>Avec consultation</Checkbox>
            </Form.Item>
            <DescriptionsItem label="Prix suggéré">
              <Price price={price} />
            </DescriptionsItem>
          </div>
          <div className="form-row-2">
            <Form.Item name={["amount", "total"]} label="Montant total">
              <Input type="number" min={0} addonBefore="TND" />
            </Form.Item>
            <Form.Item name={["amount", "paid"]} label="Montant payé">
              <Input type="number" min={0} max={totalPrice} addonBefore="TND" />
            </Form.Item>
          </div>
          <Form.Item>
            <Button
              type="primary"
              htmlType="submit"
              loading={state === "Loading" || c === "Loading"}
            >
              Confirmer
            </Button>
          </Form.Item>
        </Form>
      </Spin>
    </FocusView>
  );
};

export default ConsultationView;
