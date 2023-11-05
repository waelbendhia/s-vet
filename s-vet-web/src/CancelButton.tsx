import { useDispatch, useSelector } from "react-redux";
import { Keyed, Consultation } from "./types";
import { Button, Modal } from "antd";
import { cancelConsultationAction } from "./Consultations/state";
import { ButtonType } from "antd/lib/button";
import { SizeType } from "antd/lib/config-provider/SizeContext";

const CancelButton = ({
  consultation: c,
  type,
  size,
}: {
  consultation: Keyed<Consultation>;
  type?: ButtonType;
  size?: SizeType;
}) => {
  const cancelState = useSelector((s) => s.consultations.cancelState);
  const dispatch = useDispatch();

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
