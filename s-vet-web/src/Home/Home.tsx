import { useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import { toComponentState } from "../types";
import { getHomeData } from "./state";
import { Button, Spin, Statistic, PageHeader } from "antd";
import { openNewConsultationModal } from "../Consultations/state";
import Appointments from "../Appointments";

const Home = () => {
  const dispatch = useDispatch();
  const { statistics } = useSelector((s) => ({
    consultations: toComponentState(s.home.consultations),
    statistics: toComponentState(s.home.statistics),
  }));
  useEffect(() => {
    dispatch(getHomeData());
  }, [dispatch]);

  return (
    <div className="home-root">
      <PageHeader
        title="Aperçu"
        ghost={false}
        extra={
          <Button
            onClick={() => dispatch(openNewConsultationModal())}
            type="primary"
          >
            Nouvelle consultation
          </Button>
        }
      >
        <Spin spinning={statistics.state === "Loading"}>
          <div className="statistics">
            <Statistic
              title="Consultations restantes"
              value={statistics.data.remainingConsultations}
            />
            <Statistic
              title="Consultations terminees"
              value={statistics.data.doneConsultations}
            />
            <Statistic
              title="Revenue"
              value={statistics.data.earnings / 1000}
              precision={3}
              suffix="TND"
            />
          </div>
        </Spin>
      </PageHeader>
      <div className="home-list">
        <div className="header">
          <h2>Rendez vous</h2>
        </div>
        <Appointments />
      </div>
    </div>
  );
};

export default Home;
