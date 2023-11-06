import { useAppDispatch, useAppSelector } from "../hooks";
import { closeActModal, createActAction } from "./state";
import ActModal from "./ActModal";

const NewActModal = () => {
  const { visible, state } = useAppSelector((s) => ({
    visible: s.acts.createModalOpen,
    state: s.acts.createState,
  }));
  const dispatch = useAppDispatch();

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
