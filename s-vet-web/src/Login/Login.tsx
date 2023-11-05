import { Button, Card, Input, Form, Alert } from "antd";
import { useDispatch, useSelector } from "react-redux";
import { submitLogin } from "./state";

const Login = () => {
  const dispatch = useDispatch();
  const { account } = useSelector((s) => ({ account: s.login.loginResult }));

  return (
    <div className="login-root">
      <Card title="Connexion">
        <Form
          style={{ minWidth: "320px" }}
          onFinish={(v) => dispatch(submitLogin(v))}
        >
          <Form.Item
            label="Email"
            name="email"
            rules={[{ required: true }]}
          >
            <Input type="email" />
          </Form.Item>
          <Form.Item
            label="Mot de passe"
            name="password"
            rules={[{ required: true }]}
          >
            <Input type="password" />
          </Form.Item>
          <Form.Item>
            <Button
              type="primary"
              htmlType="submit"
              loading={account === "Loading"}
            >
              Confirmer
            </Button>
          </Form.Item>
        </Form>
        {account === "AuthenticationError" && (
          <Alert message="Mot de passe incorrecte" type="error" />
        )}
        {account === "NetworkError" && (
          <Alert
            message="Erreur de connexion, veuillez reessayer ulterieurement"
            type="error"
          />
        )}
      </Card>
    </div>
  );
};

export default Login;
