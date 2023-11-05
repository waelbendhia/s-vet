import { Form, Input, Button, Spin, Divider, Statistic } from "antd";
import moment from "moment";
import { useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { useParams } from "react-router";
import { isOk, Keyed, Owner, Pet } from "../types";
import { selectOwnerAction, updateOwnerAction } from "./state";
import FocusView from "../FocusView";
import PetPreview from "../Pets/PetPreview";

const OwnerView = () => {
  const { c, state } = useSelector((s) => ({
    c: s.owners.current,
    state: s.owners.updateState,
  }));
  const dispatch = useDispatch();
  const { key } = useParams<{ key: string }>();
  const cKey = Number.parseInt(key);
  const [form] = Form.useForm<Keyed<Owner> & { time: moment.Moment }>();
  const [pets, setPets] = useState<Keyed<Pet>[]>([]);
  const [balance, setBalance] = useState<{ paid: number; unpaid: number }>({
    paid: 0,
    unpaid: 0,
  });

  useEffect(() => {
    if (!isNaN(cKey)) {
      dispatch(selectOwnerAction(cKey));
    }
  }, [dispatch, cKey]);

  useEffect(() => {
    if (isOk(c)) {
      setPets(c.pets);
      setBalance({ paid: c.paid, unpaid: c.unpaid });
    }
  }, [c]);

  useEffect(() => {
    if (isOk(c)) {
      form.resetFields();
    }
  }, [form, c]);

  return (
    <FocusView
      title="Owner"
      subTitle={isOk(c) ? c.name : ""}
      sideContent={
        <Spin spinning={state === "Loading"}>
          <h2>Solde</h2>
          <div className="statistics">
            <Statistic
              title="Payé"
              value={balance.paid / 1000}
              precision={3}
              suffix="TND"
            />
            <Statistic
              title="Impayé"
              value={balance.unpaid / 1000}
              precision={3}
              suffix="TND"
            />
          </div>
          <Divider />
          <h2>Animaux</h2>
          <div className="owner-pets-list">
            {pets.map((p) => (
              <PetPreview pet={p} key={p.key} />
            ))}
          </div>
        </Spin>
      }
    >
      <Form
        form={form}
        initialValues={isOk(c) ? c : undefined}
        onFinish={(v) => {
          if (isOk(c)) {
            dispatch(updateOwnerAction({ ...v, key: c.key }));
          }
        }}
      >
        <Form.Item name="name" label="Nom" rules={[{ required: true }]}>
          <Input />
        </Form.Item>
        <Form.Item name="email" label="Email">
          <Input />
        </Form.Item>
        <Form.Item name="phonenumber" label="Numéro de téléphone">
          <Input />
        </Form.Item>
        <Form.Item name="address" label="Addresse">
          <Input />
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

export default OwnerView;
