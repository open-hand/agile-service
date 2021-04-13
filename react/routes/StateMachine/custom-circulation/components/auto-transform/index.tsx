import React, { useState, useCallback, useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { DataSet, CheckBox } from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { IModalProps } from '@/common/types';
import { Choerodon } from '@choerodon/boot';
import { statusTransformApi } from '@/api';
import styles from './index.less';

interface Props {
  modal: IModalProps,
  record: Record,
  selectedType: string,
  customCirculationDataSet: DataSet,
}

const AutoTransform: React.FC<Props> = ({
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const [checked, setChecked] = useState<boolean>(record.get('autoTransform'));
  const handleChange = useCallback((value) => {
    setChecked(value);
  }, []);

  useEffect(() => {
    const handleOk = async () => {
      try {
        await statusTransformApi.updateAutoTransform(selectedType, record.get('id'), checked);
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      } catch (e) {
        Choerodon.prompt('设置失败');
        return false;
      }
    };
    modal.handleOk(handleOk);
  }, [checked, customCirculationDataSet, modal, record, selectedType]);

  return (
    <div className={styles.autoTransform}>
      <CheckBox checked={checked} onChange={handleChange}>{`分支合并后自动将状态流转到${record.get('name')}`}</CheckBox>
    </div>
  );
};
export default observer(AutoTransform);
