import { Button, DatePicker, Form, Select } from "antd";
import { Link } from "react-router-dom";
import { DateTime } from "luxon";
import {
  Consultation,
  FullConsultation,
  Keyed,
  toComponentState,
  translateSpeciesWithSex,
} from "../types";
import {
  openNewConsultationModal,
  getConsultations,
  updateConsultationAction,
} from "./state";
import TableView from "../TableView";
import CancelButton from "../CancelButton";
import Price from "../Price";
import { useAppDispatch, useAppSelector } from "../hooks";

export const Consultations = () => {
  const dispatch = useAppDispatch();
  const { consultations, request } = useAppSelector((s) => ({
    consultations: toComponentState(s.consultations.consultations),
    cancelState: s.consultations.cancelState,
    request: s.consultations.searchRequest,
  }));

  const setPaid = (c: Keyed<Consultation>) =>
    !!c.amount
      ? dispatch(
          updateConsultationAction({
            ...c,
            amount: { ...c.amount, paid: c.amount.total, remaining: 0 },
          }),
        )
      : undefined;

  return (
    <TableView<FullConsultation>
      title="Consultations"
      newItem={openNewConsultationModal()}
      newItemLabel="Nouvelle consultation"
      loading={consultations.state === "Loading"}
      data={consultations.data.rows}
      total={consultations.data.total}
      page={request.page}
      itemsPerPage={request.itemsPerPage}
      searchAction={getConsultations}
      extraFilters={
        <>
          <Form.Item label="Date">
            <DatePicker.RangePicker
              mode={["date", "date"]}
              placeholder={["Début", "Fin"]}
              onChange={(v) => {
                const after = v?.[0]?.toDate();
                const before = v?.[1]?.toDate();

                dispatch(
                  getConsultations({
                    after: !!after ? DateTime.fromJSDate(after) : undefined,
                    before: !!before ? DateTime.fromJSDate(before) : undefined,
                  }),
                );
              }}
            />
          </Form.Item>
          <Form.Item className="state-filter" label="État">
            <Select
              allowClear
              onChange={(v) =>
                dispatch(
                  getConsultations({
                    status:
                      v === "pending"
                        ? "pending"
                        : v === "completed"
                        ? "completed"
                        : undefined,
                  }),
                )
              }
            >
              <Select.Option value="pending">En attente</Select.Option>
              <Select.Option value="completed">Terminé</Select.Option>
            </Select>
          </Form.Item>
        </>
      }
      columns={[
        {
          key: "time",
          title: "Date",
          render: (_, c) =>
            DateTime.fromObject(c.time).toLocaleString({
              locale: "fr",
              ...DateTime.DATE_SHORT,
            }),
        },
        {
          key: "owner.name",
          title: "Propriétaire",
          render: (_, c) => (
            <Link to={`/owners/${c.pet.owner.key}`}>{c.pet.owner.name}</Link>
          ),
        },
        {
          key: "pet.name",
          title: "Patient",
          render: (_, c) => <Link to={`/pets/${c.pet.key}`}>{c.pet.name}</Link>,
        },
        {
          key: "pet.species",
          title: "Espèce et sexe",
          render: (_, c) => translateSpeciesWithSex(c.pet),
        },
        {
          key: "motive",
          title: "Motif",
          render: (_, c) => c.motive ?? "N/A",
        },
        {
          key: "diagnosis",
          title: "Diagnostic",
          render: (_, c) => (
            <div className="diagnosis-cell">{c.diagnosis ?? "N/A"}</div>
          ),
        },
        {
          key: "amount.total",
          title: "Total",
          align: "right",
          render: (_, c) =>
            !c.amount ? "N/A" : <Price price={c.amount.total} />,
        },
        {
          key: "amount.paid",
          title: "Payé",
          align: "right",
          render: (_, c) =>
            !c.amount ? "N/A" : <Price price={c.amount.paid} />,
        },
        {
          key: "actions",
          title: "Actions",
          render: (_, c) => (
            <Button.Group className="consultations-actions">
              <CancelButton consultation={c} type="link" size="small" />
              <Button
                disabled={!c.amount || c.amount.total === c.amount.paid}
                type="link"
                size="small"
                onClick={() => setPaid(c)}
              >
                Solder
              </Button>
              <Button type="link" size="small">
                <Link to={`/consultations/${c.key}`}>
                  {!c.amount ? "Commencer" : "Modifier"}
                </Link>
              </Button>
            </Button.Group>
          ),
        },
      ]}
    />
  );
};

export default Consultations;
