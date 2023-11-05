import React from "react";
import { Form, Input, InputNumber, Modal } from "antd";
import { Act, currencyParser, Loadable, tndFormat } from "../types";
import ModalFooter from "../ModalFooter";

type ActModalProps = {
  visible: boolean;
  state: Loadable<"Ok">;
  act?: Act;
  onCancel: () => void;
  onFinish: (_: Act) => void;
  title: string;
};

const actToActForm = (a: Act): Act => ({
  price: a.price / 1000,
  name: a.name,
});

const actFormToAct = (a: Act): Act => ({
  price: Math.trunc(a.price * 1000),
  name: a.name,
});

const ActModal = ({
  visible,
  state,
  act,
  onCancel,
  onFinish,
  title,
}: ActModalProps) => {
  const [form] = Form.useForm<Act>();

  React.useEffect(() => {
    if (visible) {
      form.resetFields();
      form.setFieldsValue(!!act ? actToActForm(act) : {});
    }
  }, [visible, form, act]);

  return (
    <Modal
      zIndex={1003}
      visible={visible}
      onCancel={onCancel}
      footer={
        <ModalFooter
          onCancel={onCancel}
          loading={state === "Loading"}
          onOk={form.submit}
        />
      }
      title={title}
    >
      <Form
        form={form}
        initialValues={{ neutered: false }}
        onFinish={(v) => onFinish(actFormToAct(v))}
      >
        <Form.Item name="name" label="Nom" rules={[{ required: true }]}>
          <Input />
        </Form.Item>
        <Form.Item name="price" label="Prix">
          <InputNumber<number>
            formatter={(v) => tndFormat.format(v ?? 0)}
            parser={(v) => (!!v ? currencyParser(v) : 0)}
          />
        </Form.Item>
      </Form>
    </Modal>
  );
};

export default ActModal;
