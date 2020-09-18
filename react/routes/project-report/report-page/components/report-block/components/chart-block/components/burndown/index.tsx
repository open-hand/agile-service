import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import BurnDown from '@/components/charts/burn-down';
import useBurnDownReport from '@/components/charts/burn-down/useBurnDownReport';
import { BurnDownSearchVO } from '@/routes/project-report/report-page/store';
import { transform } from '../../../../../add-chart/components/burndown';

interface Props {
  filter: BurnDownSearchVO
}
const BurnDownComponent: React.FC<Props> = ({ filter }) => {
  const config = useMemo(() => transform(filter), [filter]);
  const [, props] = useBurnDownReport(config);
  return (
    <div>
      <BurnDown {...props} />
    </div>
  );
};
export default observer(BurnDownComponent);
