import React, { useCallback, useState, useEffect } from 'react';
import { CheckBox } from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';

import { IProjectInfo, projectApi } from '@/api';

interface Props {
  refresh: () => void
}

const HideSubTask: React.FC<Props> = ({ refresh }) => {
  const [projectInfo, setProjectInfo] = useState<IProjectInfo | undefined>();

  const getProjectInfo = useCallback(async () => {
    const res = await projectApi.loadInfo();
    setProjectInfo(res);
  }, []);
  useEffect(() => {
    getProjectInfo();
  }, [getProjectInfo]);

  const handleChange = useCallback(async (value) => {
    if (projectInfo?.infoId) {
      await projectApi.updateProject({
        infoId: projectInfo?.infoId,
        objectVersionNumber: projectInfo?.objectVersionNumber,
        hidePreSprintDoneSubissue: value,
      });
      refresh();
      getProjectInfo();
    }
  }, [getProjectInfo, projectInfo?.infoId, projectInfo?.objectVersionNumber, refresh]);

  const hidden = projectInfo?.hidePreSprintDoneSubissue;
  return (
    <Permission
      service={['agile-service.project-info.updateProjectInfo']}
      noAccessChildren={!hidden ? '' : '已隐藏历史迭代中已完成的子任务'}
    >
      <CheckBox name="hidden" value onChange={handleChange} checked={hidden}>隐藏在历史迭代已完成的子任务</CheckBox>
    </Permission>
  );
};

export default HideSubTask;
