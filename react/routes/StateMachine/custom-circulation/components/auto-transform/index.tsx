import React, {
  useState, useCallback, useEffect, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, CheckBox,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Divider } from 'choerodon-ui';
import { mount } from '@choerodon/inject';
import { IModalProps } from '@/common/types';
import { Choerodon } from '@choerodon/boot';
import { statusTransformApi } from '@/api';
import useHasTest from '@/hooks/useHasTest';
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
  const testTransformRef = useRef<{
    testTransform:(data: {
      agileIssueTypeId: string,
      agileStatusId: string,
      testStatusId: string | null
    }) => Promise<void>,
    selectedStatusId: string | undefined
  }>();
  const hasTest = useHasTest();
  const [checked, setChecked] = useState<boolean>(record.get('autoTransform'));
  const handleChange = useCallback((value) => {
    setChecked(value);
  }, []);

  useEffect(() => {
    const getAutoTransform = async () => {
      const res = await statusTransformApi.getAutoTransform(selectedType, record.get('id'));
      setChecked(res?.autoTransform);
    };
    getAutoTransform();
  }, [record, selectedType]);

  useEffect(() => {
    const handleOk = async () => {
      try {
        if (testTransformRef.current && testTransformRef.current?.testTransform) {
          await testTransformRef.current?.testTransform({
            agileIssueTypeId: selectedType,
            agileStatusId: record.get('id'),
            testStatusId: testTransformRef.current.selectedStatusId ? testTransformRef.current.selectedStatusId : null,
          });
        }
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
      {hasTest ? mount('testmanager:StatusAutoTransform', {
        testTransformRef,
        issueTypeId: selectedType,
        statusId: record.get('id'),
      }) : ''}
      {
        hasTest && (
          <>
            <div style={{
              marginTop: -12,
              fontSize: 12,
              color: 'var(--text-color3)',
              marginLeft: 2,
            }}
            >
              {`自动将issue流转到${record.get('name')}`}

            </div>
            <Divider style={{ marginTop: 30, marginBottom: 20 }} />
          </>
        )
      }
      <CheckBox checked={checked} onChange={handleChange}>{`分支合并后自动将状态流转到${record.get('name')}`}</CheckBox>
    </div>
  );
};
export default observer(AutoTransform);
