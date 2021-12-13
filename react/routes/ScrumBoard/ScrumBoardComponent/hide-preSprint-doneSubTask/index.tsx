import React, { useCallback, useState, useEffect } from 'react';
import { CheckBox } from 'choerodon-ui/pro';
import { Permission } from '@choerodon/boot';
import { C7NFormat } from '@choerodon/master';

import { IProjectInfo, projectApi } from '@/api';
import styles from './index.less';

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
      service={['choerodon.code.project.setting.general-setting.ps.update']}
      noAccessChildren={!hidden ? '' : (
        <C7NFormat
          intlPrefix="agile.scrumBoard"
          id="hidden.sub_task.completed.in.history.sprint"
        />
      )}
    >
      <CheckBox name="hidden" value onChange={handleChange} checked={hidden} className={styles.checkbox}>
        <C7NFormat
          intlPrefix="agile.scrumBoard"
          id="hide.sub_task.completed.in.history.sprint"
        />
      </CheckBox>
    </Permission>
  );
};

export default HideSubTask;
