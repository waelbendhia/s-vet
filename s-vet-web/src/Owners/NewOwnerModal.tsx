import React from "react";
import { useDispatch, useSelector } from "react-redux";
import { closeOwnerModal, createOwnerAction } from "./state";
import { Form, Input, Modal } from "antd";
import { Owner } from "../types";
import ModalFooter from "../ModalFooter";

const NewPetModal = () => {
  const { visible, state } = useSelector((s) => ({
    visible: s.owners.createModalOpen,
    state: s.owners.createState,
  }));
  const dispatch = useDispatch();
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
      visible={visible}
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
