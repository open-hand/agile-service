import React, { useMemo, useEffect, useState } from 'react';
import { Form, DataSet, Select } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import { IIssueType } from '@/common/types';
import SelectStatus from '@/components/select/select-status';
import { statusTransformApi } from '@/api';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import Loading from '@/components/Loading';
import styles from './index.less';

interface IParentIssueStatusSetting {
  id: string
  issueTypeId: string
  parentIssueStatusSetting: string
  parentIssueTypeCode: 'story' | 'task' | 'bug'
  projectId: number
  statusId: string
}

const Linkage = ({
// @ts-ignore
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const { data: issueTypes } = useProjectIssueTypes();
  const [loading, setLoading] = useState(false);
  const linkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'story', label: '指定状态', type: 'string' as FieldType, textField: 'name', valueField: 'id',
      },
      {
        name: 'task', label: '指定状态', type: 'string' as FieldType, textField: 'name', valueField: 'id',
      },
      {
        name: 'bug', label: '指定状态', type: 'string' as FieldType, textField: 'name', valueField: 'id',
      },
    ],
  }), []);

  const selectedTypeCode = find(issueTypes, (
    item: IIssueType,
  ) => item.id === selectedType)?.typeCode;

  useEffect(() => {
    const { current } = linkageDataSet;
    if (selectedTypeCode) { // 有selectedTypeCode的时候再请求，防止请求两边，浪费一次没有意义的请求
      setLoading(true);
      statusTransformApi.getLinkage(selectedType, record.get('id')).then((res: IParentIssueStatusSetting[]) => {
        setLoading(false);
        current?.set('story', find(res, { parentIssueTypeCode: 'story' })?.parentIssueStatusSetting);
        current?.set('task', find(res, { parentIssueTypeCode: 'task' })?.parentIssueStatusSetting);
        if (selectedTypeCode === 'sub_task') {
          current?.set('bug', find(res, { parentIssueTypeCode: 'bug' })?.parentIssueStatusSetting);
        }
      }).catch(() => {
        setLoading(false);
      });
    }
  }, [linkageDataSet, record, selectedType, selectedTypeCode]);

  useEffect(() => {
    const handleOk = async () => {
      const data = linkageDataSet.toData();
      // @ts-ignore
      const { story, task, bug } = data && data[0];
      const updateData = [];
      if (story) {
        updateData.push({
          parentIssueTypeCode: 'story',
          parentIssueStatusSetting: story,
        });
      }
      if (task) {
        updateData.push({
          parentIssueTypeCode: 'task',
          parentIssueStatusSetting: task,
        });
      }
      if (bug) {
        updateData.push({
          parentIssueTypeCode: 'bug',
          parentIssueStatusSetting: bug,
        });
      }
      // @ts-ignore
      await statusTransformApi.updateLinkage(selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
      customCirculationDataSet.query(customCirculationDataSet.currentPage);
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, linkageDataSet, modal, record, selectedType]);

  const getIssueTypeId = (code: string) => find(issueTypes, (
    item: IIssueType,
  ) => item.typeCode === code)?.id;

  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      <div className={styles.tip}>当工作项流转到此状态后，关联的父任务状态设置。</div>
      <Form dataSet={linkageDataSet}>
        <div>
          <p className={styles.label}>父级为故事类型</p>
          <SelectStatus name="story" key={getIssueTypeId('story')} issueTypeId={getIssueTypeId('story')} />
        </div>
        <div>
          <p className={styles.label}>父级为任务类型</p>
          <SelectStatus name="task" key={getIssueTypeId('task')} issueTypeId={getIssueTypeId('task')} />
        </div>
        {
          selectedTypeCode === 'sub_task' && (
            <div>
              <p className={styles.label}>父级为缺陷类型</p>
              <SelectStatus name="bug" key={getIssueTypeId('bug')} issueTypeId={getIssueTypeId('bug')} />
            </div>
          )
        }
      </Form>
    </div>
  );
};

export default observer(Linkage);
