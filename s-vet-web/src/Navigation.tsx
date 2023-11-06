import { Layout, Menu, Button } from "antd";
import { Link } from "react-router-dom";
import { logout } from "./Login/state";
import { useAppDispatch, useAppSelector } from "./hooks";

const Navigation = () => {
  const dispatch = useAppDispatch();
  const { account, path } = useAppSelector((s) => ({
    account: s.login.account,
    path: s.router.location?.pathname,
  }));

  return (
    <Layout.Header>
      <div className="logo-title">
        <div className="logo" />
        <div>CVAZN</div>
      </div>
      {typeof account === "object" && (
        <>
          <Menu selectedKeys={path ? [path] : []} mode="horizontal">
            <Menu.Item key="/">
              <Link to="/">Accueil</Link>
            </Menu.Item>
            <Menu.Item key="/consultations">
              <Link to="/consultations">Consultations</Link>
            </Menu.Item>
            <Menu.Item key="/owners">
              <Link to="/owners">Propriétaires</Link>
            </Menu.Item>
            <Menu.Item key="/pets">
              <Link to="/pets">Patients</Link>
            </Menu.Item>
            <Menu.Item key="/settings">
              <Link to="/settings">Paramètres</Link>
            </Menu.Item>
            <Menu.Item key="/dashboard">
              <Link to="/dashboard">Tableau de bord</Link>
            </Menu.Item>
          </Menu>
          <Button onClick={() => dispatch(logout())}>Deconnecter</Button>
        </>
      )}
    </Layout.Header>
  );
};

export default Navigation;
