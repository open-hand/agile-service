import React from 'react';
import Sprint from '@/components/charts/sprint';
import useSprintReport, { SprintConfig } from '@/components/charts/sprint/useSprintReport';

interface Props {
  filter: SprintConfig
}
const SprintComponent: React.FC<Props> = ({ filter }) => {
  const [, props] = useSprintReport(filter);
  return (
    <div>
      <Sprint {...props} />
    </div>
  );
};
export default SprintComponent;
