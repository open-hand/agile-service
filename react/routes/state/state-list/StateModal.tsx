import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  Modal, TextField, DataSet, Select, Form,
} from 'choerodon-ui/pro';
import TextArea from '@/components/TextArea';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import { statusApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { getStageList } from '@/utils/stateMachine';
import styles from './StateModal.less';

interface IStatus {
  description: string
  name: string,
  type: string,
  objectVersionNumber: number,
  statusId: string
}

const { Option } = Select;
interface Props {
  modal?: IModalProps,
  statusId?: string,
  name?: string,
  onOk: () => void
  disabledEditName?: boolean
}

const stageList = getStageList();

const CreateState: React.FC<Props> = ({
  modal, onOk, statusId, name: statusName, disabledEditName,
}) => {
  const [editStatus, setEditStatus] = useState<IStatus>();
  const checkStateNameRepeat = useCallback(async (value: string) => {
    if (value && value.trim()) {
      const res = await statusApi.checkName(value);
      if (res?.statusExist && value !== statusName) {
        return '状态名称重复';
      }
    }
    return true;
  }, [statusName]);

  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'name',
      label: '状态名称',
      type: 'string' as FieldType,
      required: true,
      validator: checkStateNameRepeat,
      maxLength: 15,
      disabled: disabledEditName,
    }, {
      name: 'description',
      label: '状态描述',
      type: 'string' as FieldType,
    }, {
      name: 'type',
      label: '阶段',
      type: 'string' as FieldType,
      textField: 'name',
      valueField: 'code',
      required: true,
    }],
  }), [checkStateNameRepeat, disabledEditName]);

  const handleSubmit = useCallback(async () => {
    const validate = await ds.validate();
    if (validate) {
      const data = {
        description: ds?.current?.get('description') as string,
        name: ds?.current?.get('name') as string,
        type: ds?.current?.get('type') as string,
      };
      statusId ? await statusApi.update(statusId as string, Object.assign(editStatus, data)) : await statusApi.create(data);
      onOk();
      return true;
    }
    return false;
  }, [ds, statusId, editStatus, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    if (statusId) {
      statusApi.load(statusId)
        .then((res: IStatus) => {
          const {
            description, type, name,
          } = res;
        ds?.current?.set('description', description);
        ds?.current?.set('name', name);
        ds?.current?.set('type', type);
        setEditStatus(res);
        });
    } else {
      ds?.current?.set('type', 'todo');
    }
  }, [statusId, ds]);
  return (
    <Form dataSet={ds} className={styles.statusModal}>
      <TextField name="name" placeholder="请输入状态名称，例如：测试中" help={disabledEditName ? '状态被需求池使用，不可更改名称' : ''} />
      <TextArea
        maxLength={45}
        name="description"
        autoSize={{
          minRows: 2,
        }}
      />
      <Select name="type" clearButton={false}>
        {stageList.map((stage) => (
          <Option
            value={stage.code}
            key={stage.code}
          >
            <div className={styles.stageOption}>
              <div className={styles.stageOption_block} style={{ backgroundColor: stage.colour }} />
              <span className={styles.stageOption_name}>{stage.name}</span>
            </div>
          </Option>
        ))}
      </Select>
    </Form>
  );
};

const ObserverCreateState = observer(CreateState);

const openStateModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: props.statusId ? '修改状态' : '创建状态',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: (
      <ObserverCreateState
        {...props}
      />
    ),
  });
};

export default openStateModal;
