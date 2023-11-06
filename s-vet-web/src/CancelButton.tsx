import { Keyed, Consultation } from "./types";
import { Button, Modal } from "antd";
import { cancelConsultationAction } from "./Consultations/state";
import { ButtonType } from "antd/lib/button";
import { SizeType } from "antd/lib/config-provider/SizeContext";
import { useAppDispatch, useAppSelector } from "./hooks";

const CancelButton = ({
  consultation: c,
  type,
  size,
}: {
  consultation: Keyed<Consultation>;
  type?: ButtonType;
  size?: SizeType;
}) => {
  const cancelState = useAppSelector((s) => s.consultations.cancelState);
  const dispatch = useAppDispatch();

  const text = !c.amount ? "annuler" : "supprimer";

  const confirmDelete = () =>
    Modal.confirm({
      title: `Êtes-vous sûr de vouloir ${text} cette consultation?`,
      cancelText: "Non",
      okText: "Oui",
      onOk: () => dispatch(cancelConsultationAction(c.key)),
    });

  return (
    <Button
      danger
      onClick={confirmDelete}
      loading={cancelState === "Loading"}
      type={type}
      size={size}
    >
      {text}
    </Button>
  );
};

export default CancelButton;
