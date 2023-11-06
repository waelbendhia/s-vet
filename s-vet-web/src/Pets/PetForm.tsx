import { DatePicker, Form, FormInstance, Input, Select } from "antd";
import { Owned, Pet } from "../types";
import OwnerSelect from "../Owners/OwnerSelect";
import Checkbox from "antd/lib/checkbox/Checkbox";

type PetValue = Omit<Owned<Pet>, "age"> & { age: moment.Moment };

type Props = {
  form?: FormInstance<PetValue>;
  onFinish?: (_: PetValue) => void;
};

const PetForm = ({ form, onFinish }: Props) => (
  <Form layout="vertical" form={form} initialValues={{ neutered: false }} onFinish={onFinish}>
    <Form.Item name="owner" label="Propriétaire" rules={[{ required: true }]}>
      <OwnerSelect />
    </Form.Item>
    <Form.Item name="name" label="Nom" rules={[{ required: true }]}>
      <Input />
    </Form.Item>
    <div className="form-row-2">
      <Form.Item
        name="age"
        label="Date de naissance"
        rules={[{ required: true }]}
      >
        <DatePicker />
      </Form.Item>
      <Form.Item name="sex" label="Sexe" rules={[{ required: true }]}>
        <Select>
          <Select.Option value="Female">Femelle</Select.Option>
          <Select.Option value="Male">Male</Select.Option>
        </Select>
      </Form.Item>
    </div>
    <div className="form-row-2">
      <Form.Item name="species" label="Espèce" rules={[{ required: true }]}>
        <Select>
          <Select.Option value="Cat">Chat</Select.Option>
          <Select.Option value="Dog">Chien</Select.Option>
        </Select>
      </Form.Item>
      <Form.Item name="breed" label="Race">
        <Input />
      </Form.Item>
    </div>
    <div className="form-row-2">
      <Form.Item name="neutered" label="Castré">
        <Checkbox />
      </Form.Item>
    </div>
  </Form>
);

export default PetForm;
