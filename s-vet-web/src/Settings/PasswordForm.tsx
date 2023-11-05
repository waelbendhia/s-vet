import { Form, Input, Button, Alert } from "antd";
import { useEffect, useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import { isOk } from "../types";
import { updatePasswordAction } from "./state";

type PasswordFormData = {
  oldPassword: string;
  newPassword: string;
  verifyPassword: string;
};

const PasswordForm = () => {
  const state = useSelector((s) => s.settings.changePasswordState);
  const dispatch = useDispatch();
  const [form] = Form.useForm();
  const [newPassword, setNewPassword] = useState<string | undefined>(undefined);

  useEffect(() => {
    if (isOk(state)) {
      form.resetFields();
    }
  }, [form, state]);

  return (
    <div className="change-password">
      <h2>Changer Mot de Passe</h2>
      <Form<PasswordFormData>
        form={form}
        onValuesChange={(_, v) => {
          setNewPassword(v.newPassword);
        }}
        onFinish={({ verifyPassword, ...req }) => {
          dispatch(updatePasswordAction(req));
        }}
      >
        <Form.Item
          label="Ancien mot de passe"
          name="oldPassword"
          rules={[{ required: true, message: "Champ obligatoire" }]}
        >
          <Input type="password" />
        </Form.Item>
        <Form.Item
          label="Nouveau mot de passe"
          name="newPassword"
          rules={[{ required: true, message: "Champ obligatoire" }, { min: 3 }]}
        >
          <Input type="password" />
        </Form.Item>
        <Form.Item
          label="Verifer mot de passe"
          name="verifyPassword"
          rules={[
            { required: true, message: "Champ obligatoire" },
            {
              message: "Les mots de passe ne correspondent pas",
              validator: (_, v) =>
                v === newPassword ? Promise.resolve() : Promise.reject(),
            },
          ]}
        >
          <Input type="password" />
        </Form.Item>
        {state === "NetworkError" && (
          <Alert
            message="Erreur de changement, veuillez reessayer"
            type="error"
          />
        )}
        <Form.Item style={{ marginBottom: 0 }}>
          <Button
            type="primary"
            htmlType="submit"
            loading={state === "Loading"}
          >
            Confirmer
          </Button>
        </Form.Item>
      </Form>
    </div>
  );
};

export default PasswordForm;
