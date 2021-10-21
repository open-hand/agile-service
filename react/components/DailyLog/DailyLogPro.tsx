import React, {
  useMemo, useState, useEffect,
  useCallback,
} from 'react';
import { Choerodon } from '@choerodon/boot';
import moment from 'moment';

import {
  DataSet, DateTimePicker, Form, Col, Row, Select, Modal, SelectBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { observer } from 'mobx-react-lite';
import { workLogApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectNumber from '@/components/select/select-number';
import './DailyLog.less';
import CKEditor from '@/components/CKEditor';
import { IModalProps } from '@/common/types';
import styles from './index.less';
import { processBeforeData } from './utils';
import {getProjectId} from "@/utils/common";

interface RecordWorkModalProps {
  issueId: string
  projectId?:string
  onOk?: () => void

}
const { Option } = SelectBox;

interface SelectTimeWithUnitProps extends Partial<Omit<SelectProps, 'name'>> {
  timeName: string
  unitName: string
}
const SelectTimeWithUnit: React.FC<SelectTimeWithUnitProps> = observer(({ timeName, unitName, record }) => {
  const unitOptions = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: false,
    data: [{ meaning: '小时', value: 'H' },
      { meaning: '天', value: 'D' },
      { meaning: '周', value: 'W' },
    ],
  }), []);

  return (
    <Row>
      <Col span={15}>
        <SelectNumber name={timeName} style={{ width: '100%' }} />
      </Col>
      <Col span={9} style={{ paddingLeft: '.18rem' }}>
        <Select
          name={unitName}
          options={unitOptions}
          clearButton={false}
          // disabled={disabled}
          style={{ width: '100%' }}
        />
      </Col>
    </Row>
  );
});
const RecordWorkLog: React.FC<{ modal?: IModalProps } & RecordWorkModalProps> = ({
  modal, projectId, issueId, onOk,
}) => {
  const [uploading, setUploading] = useState(false);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'spendTime', label: '耗费时间', required: true, type: 'number' as FieldType, pattern: /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/,
      },
      {
        name: 'timeUnit', label: '单位', type: 'string' as FieldType, defaultValue: 'H',
      },
      {
        name: 'startDate', label: '工作日期', required: true, type: 'dateTime' as FieldType, defaultValue: moment(),
      },
      {
        name: 'remain', label: '剩余的估计', type: 'string' as FieldType, defaultValue: 'auto',
      },
      { name: 'description', label: '描述', type: 'string' as FieldType },
    ],
  }), []);
  useEffect(() => {
    ['reduce', 'direct'].forEach((item) => {
      dataSet.current?.addField(`${item}_time`, {
        type: 'number' as FieldType,
        label: '时间',
        pattern: /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/,
        computedProps: {
          disabled: ({ record }) => {
            if (record.get('remain') === item) {
              return false;
            }
            return true;
          },
        },
      });
      dataSet.current?.addField(`${item}_unit`, {
        type: 'string' as FieldType,
        label: '单位',
        // defaultValue: 'H',
        computedProps: {
          disabled: ({ record }) => {
            if (record.get('remain') === item) {
              return false;
            }
            return true;
          },
        },
      });
      dataSet.current?.init(`${item}_unit`, 'H');
    });
  }, [dataSet]);

  const handleSubmit = useCallback(async () => {
    if (!await dataSet.validate()) {
      return false;
    }
    if (uploading) {
      Choerodon.prompt('请等待图片上传完成');
      return false;
    }
    const data = { issueId, ...processBeforeData(dataSet.toJSONData()[0]), projectId: projectId ?? getProjectId() };
    await workLogApi.project(projectId).create(data);
    onOk && onOk();

    return true;
  }, [dataSet, issueId, onOk, projectId, uploading]);
  useEffect(() => { modal?.handleOk(handleSubmit); }, [handleSubmit, modal]);
  return (
    <Form dataSet={dataSet} className={styles.form}>
      <SelectTimeWithUnit timeName="spendTime" unitName="timeUnit" />
      <DateTimePicker name="startDate" />
      <SelectBox
        name="remain"
        vertical
        className={styles.remain}
        onOption={() => ({
          className: styles.remain_option,
          style: {
            display: 'flex',
            marginTop: 0,
            marginBottom: 15,
          },
        })}
      >
        <Option value="auto">自动调整</Option>
        <Option value="none">不设置预估时间</Option>
        <Option value="direct">
          <span className={styles.remain_row}>
            <span>设置为</span>
            <SelectTimeWithUnit timeName="direct_time" unitName="direct_unit" />
          </span>
        </Option>
        <Option value="reduce">
          <span className={styles.remain_row}>
            <span>缩减</span>
            <SelectTimeWithUnit timeName="reduce_time" unitName="reduce_unit" />
          </span>
        </Option>
      </SelectBox>
      <CKEditor
        value={dataSet.current?.get('description')}
        style={{ width: '100%', minHeight: 300 }}
        onChange={(value) => {
          dataSet.current?.set('description', value);
        }}
        onUploadChange={(v) => {
          setUploading(v);
        }}
      />
    </Form>
  );
};
function openRecordWorkLogModal(props: RecordWorkModalProps) {
  Modal.open({
    key: Modal.key(),
    title: '登记工作日志',
    style: {
      width: MODAL_WIDTH.middle,
    },
    drawer: true,
    okText: '确定',
    children: <RecordWorkLog {...props} />,

  });
}
export default openRecordWorkLogModal;
