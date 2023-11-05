import { Spin } from "antd";
import {
  allDaysHours,
  allHours,
  DayHour,
  daysOfTheWeek,
  translateWeekday,
  WeekDay,
} from "./types";

type Props = {
  cells: Partial<Record<DayHour, React.ReactNode>>;
  defaultCell?: React.ReactNode;
  loading?: boolean;
};

const formatName = (dow: WeekDay): string =>
  translateWeekday(dow).toUpperCase().slice(0, 3);

const transposeDayHours = (() => {
  let ds: DayHour[] = [];
  for (let i = 0; i < 24; i++) {
    for (let j = 0; j < 7; j++) {
      ds.push(allDaysHours[j * 24 + i]);
    }
  }
  return ds;
})();

const WeekCalendar = ({ cells, defaultCell, loading }: Props) => (
  <Spin spinning={loading}>
    <div className="week-calendar">
      <div className="week-headers">
        {daysOfTheWeek.map((dow) => (
          <div key={dow} className="week-header">
            <h3>{formatName(dow)}</h3>
          </div>
        ))}
      </div>
      <div className="week-cells">
        {transposeDayHours.map((dh) => (
          <div className="cell" key={dh}>
            {cells[dh] ?? defaultCell}
          </div>
        ))}
      </div>
      <div className="week-side">
        {allHours.map((h) => (
          <div key={h}>{h === 0 ? null : `${h}h`}</div>
        ))}
      </div>
    </div>
  </Spin>
);

export default WeekCalendar;
