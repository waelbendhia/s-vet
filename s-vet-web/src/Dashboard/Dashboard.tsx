import { useEffect } from "react";
import { toComponentState } from "../types";
import WeekCalendar from "../WeekCalendar";
import { getConsultationsByHourAction } from "./state";
import { mapPartialRecord } from "../types";
import { DatePicker } from "antd";
import moment from "moment";
import Price from "../Price";
import { useAppDispatch, useAppSelector } from "../hooks";

const toMoment = (v?: string): moment.Moment | null =>
  !!v ? moment(v, "YYYY-MM-DD") : null;

const Dashboard = () => {
  const dispatch = useAppDispatch();
  const {
    stats: { data, state },
    req,
  } = useAppSelector((s) => ({
    stats: toComponentState(s.dashboard.consultationsByHour),
    req: s.dashboard.searchRequest,
  }));

  useEffect(() => {
    dispatch(getConsultationsByHourAction({}));
  }, [dispatch]);

  return (
    <div className="dashboard-root">
      <div className="stats-by-hour">
        <div className="stats-header">
          <h2>Statistiques par heures</h2>
          <DatePicker.RangePicker
            mode={["date", "date"]}
            placeholder={["Début", "Fin"]}
            defaultValue={[toMoment(req.after), toMoment(req.before)]}
            onChange={(v) =>
              dispatch(
                getConsultationsByHourAction({
                  after: v?.[0]?.format("YYYY-MM-DD"),
                  before: v?.[1]?.format("YYYY-MM-DD"),
                }),
              )
            }
          />
        </div>
        <WeekCalendar
          loading={state === "Loading"}
          cells={mapPartialRecord(
            data,
            ({ numberOfConsultations, revenue }) => (
              <div
                className={`schedule-cell-${
                  numberOfConsultations > 2 ? 2 : numberOfConsultations
                }`}
              >
                {numberOfConsultations === 1
                  ? "1 consultation"
                  : `${numberOfConsultations} consultation`}{" "}
                <Price price={revenue} />
              </div>
            ),
          )}
          defaultCell={<div className="schedule-cell-0" />}
        />
      </div>
    </div>
  );
};

export default Dashboard;
