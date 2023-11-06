import React from "react";
import { closeUpdateActModal, updateActAction } from "./state";
import { Form } from "antd";
import { Act } from "../types";
import ActModal from "./ActModal";
import { useAppDispatch, useAppSelector } from "../hooks";

const UpdateActModal = () => {
  const { selected, state } = useAppSelector((s) => ({
    selected: s.acts.selectedAct,
    state: s.acts.updateState,
  }));
  const hasSelected = !!selected;
  const dispatch = useAppDispatch();
  const [form] = Form.useForm<Act>();

  React.useEffect(() => {
    if (hasSelected) {
      form.resetFields();
      form.setFieldsValue({});
    }
  }, [hasSelected, form]);

  return (
    <ActModal
      visible={!!selected}
      act={selected}
      state={state}
      title={`Modifier l'acte "${selected?.name}"`}
      onCancel={() => dispatch(closeUpdateActModal())}
      onFinish={(v) => {
        if (!!selected) {
          dispatch(updateActAction({ key: selected.key, ...v }));
        }
      }}
    />
  );
};

export default UpdateActModal;
