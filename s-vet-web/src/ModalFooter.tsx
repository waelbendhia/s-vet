import { Button } from "antd";

type Props = {
  onCancel?: () => void;
  loading?: boolean;
  onOk?: () => void;
};

const ModalFooter = ({ onCancel, loading, onOk }: Props) => (
  <Button.Group>
    <Button onClick={onCancel}>Annuler</Button>
    <Button type="primary" loading={loading} onClick={onOk}>
      Créer
    </Button>
  </Button.Group>
);

export default ModalFooter;
