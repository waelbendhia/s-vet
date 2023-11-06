import { Button, Form, Table, Input, Modal } from "antd";
import { useEffect, useState } from "react";
import FocusView from "../FocusView";
import { Act, Keyed, toComponentState, useDebounce } from "../types";
import { deleteActAction, getSettingsAction, searchActsAction } from "./state";
import { openNewActModal, openUpdateActModal } from "../Acts/state";
import TariffsAndSchedule from "./TariffsAndSchedule";
import PasswordForm from "./PasswordForm";
import Price from "../Price";
import { useAppDispatch, useAppSelector } from "../hooks";

const SettingsView = () => {
  const {
    acts: { state: actsState, data: acts },
    searchTerm,
  } = useAppSelector((s) => ({
    acts: toComponentState(s.settings.acts),
    searchTerm: s.settings.search,
  }));
  const dispatch = useAppDispatch();
  const [search, setSearch] = useState("");
  const debouncedSearch = useDebounce(search, 200);

  useEffect(() => {
    dispatch(searchActsAction(debouncedSearch));
  }, [dispatch, debouncedSearch]);

  useEffect(() => {
    dispatch(getSettingsAction());
    dispatch(searchActsAction());
  }, [dispatch]);

  return (
    <FocusView
      backButton={false}
      title="Paramètres"
      responsive={false}
      sideContent={
        <>
          <TariffsAndSchedule />
          <PasswordForm />
        </>
      }
    >
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
        }}
      >
        <h2>Actes</h2>
        <Button onClick={() => dispatch(openNewActModal())} type="primary">
          Nouvel Acte
        </Button>
      </div>
      <Table<Keyed<Act>>
        loading={actsState === "Loading"}
        dataSource={acts}
        title={() => (
          <Form.Item label="Recherche">
            <Input
              defaultValue={searchTerm}
              onChange={(e) => setSearch(e.target.value)}
            />
          </Form.Item>
        )}
        size="small"
        pagination={false}
        columns={[
          { key: "name", title: "Nom", render: (a) => a.name },
          {
            key: "price",
            title: "Prix",
            render: (_, a) => <Price price={a.price} />,
          },
          {
            key: "actions",
            title: "Actions",
            width: "220px",
            render: (_, a) => (
              <>
                <Button
                  danger
                  type="link"
                  size="small"
                  onClick={() =>
                    Modal.confirm({
                      title: `Êtes-vous sûr de vouloir supprimer l'acte "${a.name}"?`,
                      cancelText: "Non",
                      okText: "Oui",
                      onOk: () => dispatch(deleteActAction(a.key)),
                    })
                  }
                >
                  Supprimer
                </Button>
                <Button
                  type="link"
                  size="small"
                  onClick={() => dispatch(openUpdateActModal(a))}
                >
                  Modifier
                </Button>
              </>
            ),
          },
        ]}
      />
    </FocusView>
  );
};

export default SettingsView;
