import React, { useMemo, useCallback, useImperativeHandle } from 'react';
import { EmptyPage } from '@choerodon/components';
import VersionBurnDown from '@/components/charts/version-burnDown';
import VersionBurnDownSearch from '@/components/charts/version-burnDown/search';
import useVersionBurnDownReport, { VersionBurnConfig } from '@/components/charts/version-burnDown/useVersionBurnDownReport';
import pic from '@/assets/image/NoData.svg';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { IReportChartBlock, VersionBurndownSearchVO } from '@/routes/project-report/report-page/store';
import { getProjectId } from '@/utils/common';
import { ChartRefProps } from '../..';

interface Props {
  innerRef: React.MutableRefObject<ChartRefProps>
  data?: IReportChartBlock
  projectId?: string
  isProgram?: boolean
  linkTo: (url: string) => void,
}
export const transformVersionBurndownSearch = (searchVO: VersionBurndownSearchVO | undefined): VersionBurnConfig | undefined => {
  if (!searchVO) {
    return undefined;
  }
  return ({
    versionId: searchVO.versionId,
    checked: searchVO.calibrationSprint ? 'checked' : undefined,
    projectId: searchVO.projectId,
  });
};

const EpicBurnDownComponent:React.FC<Props> = ({
  innerRef, projectId, data, isProgram, linkTo,
}) => {
  const config = useMemo(() => ({
    ...transformVersionBurndownSearch(data?.chartSearchVO as VersionBurndownSearchVO),
    projectId,
  }), [data?.chartSearchVO, projectId]);
  const [searchProps, props] = useVersionBurnDownReport(config);
  const { versions, currentVersionId, checked } = searchProps;
  const handleSubmit = useCallback(async (): Promise<VersionBurndownSearchVO> => ({
    type: 'version',
    calibrationSprint: checked === 'checked',
    versionId: currentVersionId,
    projectId: searchProps.projectId || getProjectId(),
  }),
  [checked, currentVersionId, searchProps.projectId]);

  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);

  return (
    <div>
      {
        versions && versions.length > 0 ? (
          <>
            <VersionBurnDownSearch {...searchProps} />
            <VersionBurnDown {...props} />
          </>
        ) : (
          <EmptyPage
            image={pic}
            description={(
              <div>
                {isProgram ? (
                  <span>当前项目无可用版本，请在【版本列表】中创建一个版本</span>
                ) : (
                  <>
                    <span>当前项目无可用版本，请在</span>
                    <EmptyPage.Button
                      onClick={() => {
                        linkTo(LINK_URL.workListVersion);
                      }}
                    >
                      【版本列表】
                    </EmptyPage.Button>
                    <span>中创建一个版本</span>
                  </>
                )}
              </div>
          )}
          />
        )
      }
    </div>
  );
};
export default EpicBurnDownComponent;
