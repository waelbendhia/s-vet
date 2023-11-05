import React from "react";
import { useDispatch, useSelector } from "react-redux";
import { closeConsultationModal, createConsultationAction } from "./state";
import { Button, DatePicker, Form, Input, Modal } from "antd";
import { Keyed, Owned, Pet } from "../types";
import { DateTime } from "luxon";
import PetSelect from "../Pets/PetSelect";
import moment from "moment";
import { useStateWithPromise } from "../selectors";

const NewConsultationModal = () => {
  const { visible, state } = useSelector((s) => ({
    visible: s.consultations.createModalOpen,
    state: s.consultations.createState,
  }));
  const loading = state === "Loading";
  const dispatch = useDispatch();
  const [startImmediately, setStartImmediately] = useStateWithPromise(false);
  const [form] = Form.useForm<{
    patient: Owned<Keyed<Pet>>;
    motive: string;
    time: moment.Moment;
  }>();

  const submit = (immediate: boolean) => () =>
    setStartImmediately(immediate).then(form.submit);

  React.useEffect(() => {
    if (visible) {
      form.resetFields();
      form.setFieldsValue({
        time: moment(),
      });
    }
  }, [visible, form]);

  return (
    <Modal
      visible={visible}
      onCancel={() => dispatch(closeConsultationModal())}
      footer={
        <Button.Group>
          <Button onClick={() => dispatch(closeConsultationModal())}>
            Annuler
          </Button>
          <Button loading={loading} onClick={submit(false)}>
            Créer
          </Button>
          <Button type="primary" loading={loading} onClick={submit(true)}>
            Commencer
          </Button>
        </Button.Group>
      }
      title="Nouvelle consultation"
    >
      <Form
        form={form}
        onFinish={(v) => {
          dispatch(
            createConsultationAction({
              startImmediately,
              consultation: {
                pet: v.patient,
                motive: v.motive,
                time: DateTime.fromJSDate(v.time.toDate()),
                treatment: [],
              },
            })
          );
        }}
      >
        <Form.Item name="patient" label="Patient" rules={[{ required: true }]}>
          <PetSelect />
        </Form.Item>
        <Form.Item name="motive" label="Motif" rules={[{ required: true }]}>
          <Input />
        </Form.Item>
        <Form.Item name="time" label="Date" rules={[{ required: true }]}>
          <DatePicker />
        </Form.Item>
      </Form>
    </Modal>
  );
};

export default NewConsultationModal;
