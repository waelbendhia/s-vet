import React from "react";

export const Descriptions = ({ children = null as React.ReactNode }) => (
  <div className="descriptions">{children}</div>
);

export const DescriptionsItem = ({
  children = null as React.ReactNode,
  label = null as React.ReactNode,
  span = 1,
}) => (
  <div className="descriptions-item" style={{ gridColumn: `span ${span}` }}>
    {label && <div className="descriptions-label">{label}</div>}
    {label && <div className="descriptions-value">{children}</div>}
  </div>
);
