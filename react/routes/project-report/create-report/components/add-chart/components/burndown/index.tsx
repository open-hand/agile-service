import React, { useCallback, useImperativeHandle } from 'react';
import BurnDown from '@/components/charts/burn-down';
import BurnDownSearch from '@/components/charts/burn-down/search';
import useBurnDownReport, { BurnDownConfig } from '@/components/charts/burn-down/useBurnDownReport';
import { ChartRefProps } from '../..';

interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
}
const BurnDownComponent: React.FC<Props> = ({ innerRef }) => {
  const [searchProps, props] = useBurnDownReport();
  const handleSubmit = useCallback(async (): Promise<BurnDownConfig> => ({
    type: searchProps.type,
    restDayShow: searchProps.restDayShow,
    sprintId: searchProps.sprintId,
    quickFilter: searchProps.quickFilter,
  }),
  [searchProps]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <BurnDownSearch {...searchProps} />
      <BurnDown {...props} />
    </div>
  );
};
export default BurnDownComponent;
