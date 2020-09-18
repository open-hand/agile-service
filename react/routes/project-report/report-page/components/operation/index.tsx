import React, { useCallback } from 'react';
import { toJS } from 'mobx';
import { Button } from 'choerodon-ui/pro';
import { FuncType, ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { projectReportApi, IProjectReportCreate, IProjectReportUpdate } from '@/api';
import { getProjectId } from '@/utils/common';
import to from '@/utils/to';
import styles from './index.less';
import { useProjectReportContext } from '../../context';

interface Props {

}
const Operation: React.FC<Props> = () => {
  const { store, baseInfoRef, edit } = useProjectReportContext();
  const handleSubmit = useCallback(async () => {
    const baseInfo = await baseInfoRef.current.submit();
    if (baseInfo && baseInfo instanceof Object) {
      if (edit) {
        const data: IProjectReportUpdate = {
          ...baseInfo,
          objectVersionNumber: store.baseInfo?.objectVersionNumber,
          projectId: getProjectId(),
          reportUnitList: toJS(store.blockList),
        } as IProjectReportUpdate;
        await projectReportApi.update(store?.baseInfo?.id as string, data);
      } else {
        const data: IProjectReportCreate = {
          ...baseInfo,
          projectId: getProjectId(),
          reportUnitList: toJS(store.blockList),
        } as IProjectReportCreate;
        await projectReportApi.create(data);
      }
      to('/agile/project-report');
    }
  }, [baseInfoRef, edit, store.baseInfo?.id, store.baseInfo?.objectVersionNumber, store.blockList]);
  return (
    <div
      className={styles.bar}
    >
      <Button funcType={'raised' as FuncType} color={'blue' as ButtonColor} onClick={handleSubmit}>保存</Button>
      <Button funcType={'raised' as FuncType}>预览</Button>
      <Button funcType={'raised' as FuncType}>导出</Button>
      <Button funcType={'raised' as FuncType}>发送</Button>
      <Button funcType={'raised' as FuncType}>取消</Button>
    </div>
  );
};

export default Operation;
