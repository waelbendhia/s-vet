import {
  Form,
  Input,
  Button,
  Spin,
  Divider,
  Empty,
  DatePicker,
  Select,
  Checkbox,
} from "antd";
import moment from "moment";
import { useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useParams } from "react-router";
import { Consultation, isOk, Keyed, Owner, Pet } from "../types";
import { selectPetAction, updatePetAction } from "./state";
import FocusView from "../FocusView";
import OwnerPreview from "../Owners/OwnerPreview";
import ConsultationPreview from "../Consultations/ConsultationPreview";
import { DateTime } from "luxon";

const PetView = () => {
  const { c, state } = useSelector((s) => ({
    c: s.pets.current,
    state: s.pets.updateState,
  }));
  const dispatch = useDispatch();
  const { key } = useParams<{ key: string }>();
  const cKey = Number.parseInt(key);
  const [form] = Form.useForm<Keyed<Pet> & { age: moment.Moment }>();
  const [consultations, setConsultaions] = useState<Keyed<Consultation>[]>([]);
  const [owner, setOwner] = useState<Keyed<Owner>>({ key: 0, name: "" });

  useEffect(() => {
    if (!isNaN(cKey)) {
      dispatch(selectPetAction(cKey));
    }
  }, [dispatch, cKey]);

  useEffect(() => {
    if (isOk(c)) {
      setConsultaions(c.consultations);
      setOwner(c.owner);
    }
  }, [c]);

  useEffect(() => {
    if (isOk(c)) {
      form.resetFields();
    }
  }, [form, c]);

  return (
    <FocusView
      title="Pet"
      subTitle={isOk(c) ? c.name : ""}
      sideContent={
        <Spin spinning={state === "Loading"}>
          <h2>Propriétaire</h2>
          <OwnerPreview owner={owner} />
          <Divider />
          <h2>Consultations</h2>
          <div className="previous-list">
            {consultations.length === 0 ? (
              <Empty description="Aucune consultation précédente" />
            ) : (
              consultations.map((p) => (
                <ConsultationPreview consultation={p} key={p.key} />
              ))
            )}
          </div>
        </Spin>
      }
    >
      <Form
        form={form}
        initialValues={
          isOk(c)
            ? {
                ...c,
                age: moment(DateTime.fromObject(c.age).toJSDate()),
              }
            : undefined
        }
        onFinish={({ age, ...v }) => {
          if (isOk(c)) {
            dispatch(
              updatePetAction({
                ...v,
                age: DateTime.fromJSDate(age.toDate()).toObject(),
                allergies: [],
                key: c.key,
              })
            );
          }
        }}
      >
        <Form.Item name="name" label="Nom" rules={[{ required: true }]}>
          <Input />
        </Form.Item>
        <Form.Item
          name="age"
          label="Date de naissance"
          rules={[{ required: true }]}
        >
          <DatePicker />
        </Form.Item>
        <div className="form-row-2">
          <Form.Item name="sex" label="Sexe" rules={[{ required: true }]}>
            <Select>
              <Select.Option value="Female">Male</Select.Option>
              <Select.Option value="Male">Femelle</Select.Option>
            </Select>
          </Form.Item>
          <Form.Item name="species" label="Espèce" rules={[{ required: true }]}>
            <Select>
              <Select.Option value="Cat">Chat</Select.Option>
              <Select.Option value="Dog">Chien</Select.Option>
            </Select>
          </Form.Item>
        </div>
        <Form.Item name="breed" label="Race">
          <Input />
        </Form.Item>
        <Form.Item name="neutered" label="Castré">
          <Checkbox />
        </Form.Item>
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
    </FocusView>
  );
};

export default PetView;
