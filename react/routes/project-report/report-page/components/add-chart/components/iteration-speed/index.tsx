import React, { useMemo, useImperativeHandle, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import IterationSpeedSearch from '@/components/charts/iteration-speed/search';
import IterationSearch from '@/components/charts/iteration-speed';
import useIterationSpeedReport, { IterationSpeedConfig } from '@/components/charts/iteration-speed/useIterationSpeedReport';
import { IReportChartBlock, IterationSpeedSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';
import { validateSearchDataBySearchProps } from '../../utils';

export const transformIterationSpeedSearch = (searchVO: IterationSpeedSearchVO | undefined) : IterationSpeedConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    unit: searchVO.type,
    projectId: searchVO.projectId,
  });
};
interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
const IterationSpeedComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformIterationSpeedSearch(data?.chartSearchVO as IterationSpeedSearchVO),
    projectId,
    openValidate: true,
  }), [data?.chartSearchVO, projectId]);
  const [props, searchProps] = useIterationSpeedReport(config);

  const handleSubmit = useCallback(async (): Promise<IterationSpeedSearchVO> => validateSearchDataBySearchProps(searchProps, ({
    type: searchProps.unit,
    projectId: searchProps.projectId || getProjectId(),
  })),
  [searchProps]);

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
