import { FormattedNumber } from "react-intl";

const Price = ({ price }: { price: number }) => (
  // eslint-disable-next-line react/style-prop-object
  <FormattedNumber value={price / 1000} currency="TND" style="currency" />
);

export default Price;
