import React, { useMemo, useImperativeHandle, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import IterationSpeedSearch from '@/components/charts/iteration-speed/search';
import IterationSearch from '@/components/charts/iteration-speed';
import useIterationSpeedReport from '@/components/charts/iteration-speed/useIterationSpeedReport';
import { IReportChartBlock, IterationSpeedSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';

export const transformIterationSpeedSearch = (searchVO: IterationSpeedSearchVO) => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
}
const IterationSpeedComponent:React.FC<Props> = ({ innerRef, data }) => {
  const config = useMemo(() => transformIterationSpeedSearch(data?.chartSearchVO as IterationSpeedSearchVO), [data?.chartSearchVO]);
  const [props, searchProps] = useIterationSpeedReport(config);

  const handleSubmit = useCallback(async (): Promise<IterationSpeedSearchVO> => ({
    type: searchProps.unit,
    projectId: getProjectId(),
  }),
  [searchProps.unit]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <IterationSpeedSearch {...searchProps} />
      <IterationSearch {...props} />
    </div>
  );
};

export default observer(IterationSpeedComponent);
