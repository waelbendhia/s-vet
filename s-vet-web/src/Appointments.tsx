import { Link } from "react-router-dom";
import { toComponentState, translateSpeciesWithSex } from "./types";
import { Button, List } from "antd";
import CancelButton from "./CancelButton";
import { SizeType } from "antd/lib/config-provider/SizeContext";
import { useEffect } from "react";
import { getHomeData } from "./Home/state";
import { useAppDispatch, useAppSelector } from "./hooks";

const Appointments = ({ size = "medium" as SizeType }) => {
  const dispatch = useAppDispatch();
  const { consultations } = useAppSelector((s) => ({
    consultations: toComponentState(s.home.consultations),
  }));

  useEffect(() => {
    if (consultations.state === "NotRequested") {
      dispatch(getHomeData());
    }
  }, [dispatch, consultations.state]);

  return (
    <List
      loading={consultations.state === "Loading"}
      dataSource={consultations.data}
      locale={{ emptyText: "Aucune consultation" }}
      renderItem={(c) => (
        <List.Item>
          <div className={`appointment-overview ${size}`}>
            <div className={`appointment-description ${size}`}>
              <Link to={`/owners/${c.pet.owner.key}`}>
                <b>{c.pet.owner.name}</b>
              </Link>{" "}
              et {c.pet.sex === "Male" ? "son" : "sa"}{" "}
              <b>{translateSpeciesWithSex(c.pet)}</b>{" "}
              <Link to={`/pets/${c.pet.key}`}>
                <b>{c.pet.name}</b>
              </Link>{" "}
              pour <b>{c.motive}</b>
            </div>
            <Button.Group>
              {size !== "small" && (
                <CancelButton size={size} consultation={c} />
              )}
              <Button size={size} type={size === "small" ? "link" : undefined}>
                <Link to={`/consultations/${c.key}`}>
                  {!c.amount ? "Commencer" : "Modifier"}
                </Link>
              </Button>
            </Button.Group>
          </div>
        </List.Item>
      )}
    />
  );
};

export default Appointments;
