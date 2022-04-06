import React, {
  useState, useCallback, useEffect, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, CheckBox,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { mount, has as hasInject } from '@choerodon/inject';
import { Choerodon } from '@choerodon/boot';
import { IModalProps } from '@/common/types';
import { statusTransformApi } from '@/api';
import useHasTest from '@/hooks/useHasTest';
import styles from './index.less';
import { WATERFALL_TYPE_CODES } from '@/constants/TYPE_CODE';

interface Props {
  modal: IModalProps,
  record: Record,
  selectedType: string,
  customCirculationDataSet: DataSet,
  // eslint-disable-next-line react/require-default-props
  selectedTypeCode?: string,
}

const AutoTransform: React.FC<Props> = ({
  modal, record, selectedType, customCirculationDataSet, selectedTypeCode,
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
      <div className={styles.tip}>满足以下条件后工作项将自动流转到当前状态。</div>
      <div className={styles.setting}>
        {selectedTypeCode && !WATERFALL_TYPE_CODES.includes(selectedTypeCode) && hasTest ? (
          <>
            <div style={{
              fontSize: 12,
              color: 'var(--text-color3)',
              marginLeft: 2,
              marginBottom: 10,
            }}
            >
              {`工作项关联的用例的测试执行处于下方状态，工作项将自动流转到${record.get('name')}。`}
            </div>
            {
              hasInject('testmanager:StatusAutoTransform') ? mount('testmanager:StatusAutoTransform', {
                testTransformRef,
                issueTypeId: selectedType,
                statusId: record.get('id'),
              }) : ''
            }
            <div className={styles.divider} />
          </>
        ) : null}
        <CheckBox checked={checked} onChange={handleChange}>{`分支合并后自动将状态流转到${record.get('name')}`}</CheckBox>
      </div>

    </div>
  );
};
export default observer(AutoTransform);
