import "./App.less";
import { Button, Layout, Result, Spin } from "antd";
import { Route, Routes } from "react-router-dom";
import { HistoryRouter as Router } from "redux-first-history/rr6";
import Login from "./Login";
import { useAppDispatch, useAppSelector } from "./hooks";
import { history } from "./store";
import React from "react";
import { getAccount } from "./Login/state";
import Home from "./Home";
import Consultations, {
  Consultation,
  NewConsultationModal,
} from "./Consultations";
import Pets, { NewPetModal, Pet } from "./Pets";
import Owners, { NewOwnerModal, Owner } from "./Owners";
import { NewActModal, UpdateActModal } from "./Acts";
import Settings from "./Settings";
import Dashboard from "./Dashboard";
import Navigation from "./Navigation";

const App = () => {
  const account = useAppSelector((s) => s.login.account);
  const dispatch = useAppDispatch();
  React.useEffect(() => {
    if (account === "NotRequested") {
      dispatch(getAccount());
    }
  }, [account, dispatch]);

  return (
    <Router history={history}>
      <NewConsultationModal />
      <NewPetModal />
      <NewOwnerModal />
      <NewActModal />
      <UpdateActModal />
      <Layout>
        <Navigation />
        <Layout.Content>
          {typeof account === "object" ? (
            <Routes>
              <Route path="/consultations/:key" element={<Consultation />} />
              <Route path="/consultations" element={<Consultations />} />
              <Route path="/owners/:key" element={<Owner />} />
              <Route path="/owners" element={<Owners />} />
              <Route path="/pets/:key" element={<Pet />} />
              <Route path="/pets" element={<Pets />} />
              <Route path="/dashboard" element={<Dashboard />} />
              <Route path="/settings" element={<Settings />} />
              <Route path="/" element={<Home />} />
            </Routes>
          ) : account === "Outsider" ? (
            <Login />
          ) : account === "NetworkError" ? (
            <Result
              title="Erreur"
              subTitle="Une erreur est survenue, veuillez contacter Wael"
              extra={
                <Button onClick={() => dispatch(getAccount())}>
                  Reessayer
                </Button>
              }
            />
          ) : (
            <Spin className="full-page-spin" spinning />
          )}
        </Layout.Content>
      </Layout>
    </Router>
  );
};

export default App;
