import React, { useMemo, useCallback, useImperativeHandle } from 'react';
import { EmptyPage } from '@choerodon/components';
import EpicBurnDown from '@/components/charts/epic-burnDown';
import EpicBurnDownSearch from '@/components/charts/epic-burnDown/search';
import useEpicBurnDownReport, { EpicBurnConfig } from '@/components/charts/epic-burnDown/useEpicBurnDownReport';
import pic from '@/assets/image/NoData.svg';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { IReportChartBlock, EpicBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';

interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
}
export const transformEpicBurndownSearch = (searchVO: EpicBurndownSearchVO | undefined): EpicBurnConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    epicId: searchVO.epicId,
    checked: searchVO.calibrationSprint ? 'checked' : undefined,
    projectId: searchVO.projectId,
  });
};

const EpicBurnDownComponent:React.FC<Props> = ({ innerRef, projectId, data }) => {
  const config = useMemo(() => ({
    ...transformEpicBurndownSearch(data?.chartSearchVO as EpicBurndownSearchVO),
    projectId,
  }), [data?.chartSearchVO, projectId]);
  const [searchProps, props] = useEpicBurnDownReport(config);
  const { epics, currentEpicId, checked } = searchProps;
  const handleSubmit = useCallback(async (): Promise<EpicBurndownSearchVO> => ({
    type: 'epic',
    epicId: currentEpicId,
    calibrationSprint: checked === 'checked',
    projectId: searchProps.projectId || getProjectId(),
  }),
  [checked, currentEpicId, searchProps.projectId]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);

  return (
    <div>
      {
        epics && epics.length > 0 ? (
          <>
            <EpicBurnDownSearch {...searchProps} />
            <EpicBurnDown {...props} />
          </>
        ) : (
          <EmptyPage
            image={pic}
            description={(
              <div>
                <span>当前项目无可用史诗，请在</span>
                <EmptyPage.Button
                  onClick={() => {
                    to(LINK_URL.workListBacklog);
                  }}
                >
                  【待办事项】
                </EmptyPage.Button>
                <span>或</span>
                <EmptyPage.Button
                  onClick={() => {
                    to(LINK_URL.workListIssue);
                  }}
                >
                  【所有问题】
                </EmptyPage.Button>
                <span>中创建一个史诗</span>
              </div>
            )}
          />
        )
      }

    </div>
  );
};
export default EpicBurnDownComponent;
