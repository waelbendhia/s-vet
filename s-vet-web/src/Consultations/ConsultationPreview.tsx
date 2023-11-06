import { Link } from "react-router-dom";
import { Button } from "antd";
import { DateTime } from "luxon";
import { Keyed, Consultation, formatMass, treatmentName } from "../types";
import { updateConsultationAction } from "./state";
import { Descriptions, DescriptionsItem } from "../Descriptions";
import CancelButton from "../CancelButton";
import Price from "../Price";
import { useAppDispatch } from "../hooks";

const ConsultationPreview = ({
  consultation: c,
}: {
  consultation: Keyed<Consultation>;
}) => {
  const dispatch = useAppDispatch();

  const setPaid = () =>
    !!c.amount
      ? dispatch(
          updateConsultationAction({
            ...c,
            amount: { ...c.amount, paid: c.amount.total, remaining: 0 },
          })
        )
      : undefined;

  return (
    <Descriptions>
      <DescriptionsItem label="Date">
        {DateTime.fromObject(c.time).toLocaleString({
          locale: "fr",
          ...DateTime.DATETIME_SHORT,
        })}
      </DescriptionsItem>
      <DescriptionsItem label="Motif">{c.motive}</DescriptionsItem>
      <DescriptionsItem label="Diagnostic" span={2}>
        {c.diagnosis ?? "N/A"}
      </DescriptionsItem>
      <DescriptionsItem label="Traitement">
        {c.treatment.length === 0
          ? "N/A"
          : c.treatment.map(treatmentName).join(", ")}
      </DescriptionsItem>
      <DescriptionsItem label="Poid">
        {!c.weight ? "N/A" : formatMass(c.weight)}
      </DescriptionsItem>
      <DescriptionsItem label="Total">
        {!!c.amount ? <Price price={c.amount.total} /> : "N/A"}
      </DescriptionsItem>
      <DescriptionsItem label="Impayé">
        {!!c.amount ? <Price price={c.amount.remaining} /> : "N/A"}
      </DescriptionsItem>
      <Button.Group>
        <CancelButton consultation={c} type="link" size="small" />
        <Button
          disabled={!c.amount || c.amount.total === c.amount.paid}
          onClick={setPaid}
          type="link"
          size="small"
        >
          Solder
        </Button>
        <Button type="link" size="small">
          <Link to={`/consultations/${c.key}`}>
            {!c.amount ? "Commencer" : "Modifier"}
          </Link>
        </Button>
      </Button.Group>
    </Descriptions>
  );
};

export default ConsultationPreview;
