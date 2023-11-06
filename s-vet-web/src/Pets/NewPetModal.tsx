import React from "react";
import { closePetModal, createPetAction } from "./state";
import { Form, Modal } from "antd";
import { Owned, Pet } from "../types";
import { DateTime } from "luxon";
import PetForm from "./PetForm";
import ModalFooter from "../ModalFooter";
import { useAppDispatch, useAppSelector } from "../hooks";

const NewPetModal = () => {
  const { visible, state } = useAppSelector((s) => ({
    visible: s.pets.createModalOpen,
    state: s.pets.createState,
  }));
  const dispatch = useAppDispatch();
  const [form] = Form.useForm<
    Omit<Owned<Pet>, "age"> & { age: moment.Moment }
  >();

  React.useEffect(() => {
    if (visible) {
      form.resetFields();
      form.setFieldsValue({});
    }
  }, [visible, form]);

  return (
    <Modal
      zIndex={1001}
      onCancel={() => dispatch(closePetModal())}
      open={visible}
      footer={
        <ModalFooter
          onCancel={() => dispatch(closePetModal())}
          loading={state === "Loading"}
          onOk={form.submit}
        />
      }
      title="Nouveau Patient"
    >
      <PetForm
        form={form}
        onFinish={({ age, ...v }) => {
          dispatch(
            createPetAction({
              ...v,
              allergies: [],
              age: DateTime.fromJSDate(age.toDate()).toObject(),
            })
          );
        }}
      />
    </Modal>
  );
};

export default NewPetModal;
