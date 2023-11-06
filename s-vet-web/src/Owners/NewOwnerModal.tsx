import React from "react";
import { closeOwnerModal, createOwnerAction } from "./state";
import { Form, Input, Modal } from "antd";
import { Owner } from "../types";
import ModalFooter from "../ModalFooter";
import { useAppDispatch, useAppSelector } from "../hooks";

const NewPetModal = () => {
  const { visible, state } = useAppSelector((s) => ({
    visible: s.owners.createModalOpen,
    state: s.owners.createState,
  }));
  const dispatch = useAppDispatch();
  const [form] = Form.useForm<Owner>();

  React.useEffect(() => {
    if (visible) {
      form.resetFields();
      form.setFieldsValue({});
    }
  }, [visible, form]);

  return (
    <Modal
      zIndex={1002}
      onCancel={() => dispatch(closeOwnerModal())}
      open={visible}
      footer={
        <ModalFooter
          onCancel={() => dispatch(closeOwnerModal())}
          loading={state === "Loading"}
          onOk={form.submit}
        />
      }
      title="Nouveau Propriétaire"
    >
      <Form
        layout="vertical"
        form={form}
        onFinish={(o) => {
          dispatch(createOwnerAction(o));
        }}
      >
        <Form.Item name="name" label="Nom" rules={[{ required: true }]}>
          <Input />
        </Form.Item>
        <div className="form-row-2">
          <Form.Item name="email" label="Email">
            <Input />
          </Form.Item>
          <Form.Item name="phonenumber" label="Numéro de téléphone">
            <Input />
          </Form.Item>
        </div>
        <Form.Item name="address" label="Addresse">
          <Input />
        </Form.Item>
      </Form>
    </Modal>
  );
};

export default NewPetModal;
