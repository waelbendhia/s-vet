import { FormattedNumber } from "react-intl";

const Price = ({ price }: { price: number }) => (
  <FormattedNumber value={price / 1000} currency="TND" style="currency" />
);

export default Price;
