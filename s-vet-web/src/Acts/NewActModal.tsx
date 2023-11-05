import { useDispatch, useSelector } from "react-redux";
import { closeActModal, createActAction } from "./state";
import ActModal from "./ActModal";

const NewActModal = () => {
  const { visible, state } = useSelector((s) => ({
    visible: s.acts.createModalOpen,
    state: s.acts.createState,
  }));
  const dispatch = useDispatch();

  return (
    <ActModal
      visible={visible}
      state={state}
      title="Nouvelle Acte"
      onCancel={() => dispatch(closeActModal())}
      onFinish={(v) => dispatch(createActAction(v))}
    />
  );
};

export default NewActModal;
