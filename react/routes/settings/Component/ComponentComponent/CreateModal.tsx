import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  Modal, TextField, TextArea, DataSet, NumberField, Select, Form,
} from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import SelectUser from '@/components/select/select-user';
import { componentApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './CreateModal.less';

interface IComponent {
  defaultAssigneeRole: string,
  description: string
  managerId: string,
  sequence: number,
  name: string,
  objectVersionNumber: number,
  componentId: string
}

const { Option } = Select;
interface Props {
  modal?: IModalProps,
  componentId?: string,
  name?: string,
  onOk: () => void
}

const CreateComponent: React.FC<Props> = ({
  modal, onOk, componentId, name: componentName,
}) => {
  const [component, setComponent] = useState<IComponent>();
  const [, setUpdateCount] = useState<number>(0);
  const checkComponentNameRepeat = useCallback(async (value: string) => {
    if (value && value.trim()) {
      const data: boolean = await componentApi.checkName(value);
      if (data && value !== componentName) {
        return '模块名称重复';
      }
    }
    return true;
  }, [componentName]);

  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'name',
      label: '模块名称',
      type: 'string' as FieldType,
      required: true,
      validator: checkComponentNameRepeat,
      maxLength: 100,
    }, {
      name: 'description',
      label: '模块描述',
      type: 'string' as FieldType,
      maxLength: 30,
    }, {
      name: 'sequence',
      label: '模块顺序',
      type: 'number' as FieldType,
      min: 1,
      max: 100000,
      step: 1,
    }, {
      name: 'defaultAssigneeRole',
      label: '默认经办人',
      type: 'string' as FieldType,
      required: true,
    }, {
      name: 'managerId',
      label: '负责人',
      type: 'string' as FieldType,
      textField: 'realName',
      valueField: 'id',
    }],
    events: {
      update: ({ name: updateName, dataSet }: { name: string, dataSet: DataSet}) => {
        if (updateName === 'defaultAssigneeRole') {
          setUpdateCount((count) => count + 1);
          dataSet?.current?.set('managerId', undefined);
        }
      },
    },
  }), [checkComponentNameRepeat]);

  const handleSubmit = useCallback(async () => {
    const validate = await ds.validate();
    if (validate) {
      const data = {
        defaultAssigneeRole: ds?.current?.get('defaultAssigneeRole') as string,
        description: ds?.current?.get('description') as string,
        managerId: ds?.current?.get('managerId') || '0' as string,
        name: ds?.current?.get('name') as string,
        sequence: ds?.current?.get('sequence') as number,
        componentId,
        objectVersionNumber: componentId ? component?.objectVersionNumber : undefined,
      };
      componentId ? await componentApi.update(componentId as string, data) : await componentApi.create(data);
      onOk();
      return true;
    }
    return false;
  }, [component?.objectVersionNumber, componentId, ds, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    if (componentId) {
      componentApi.load(componentId)
        .then((res: IComponent) => {
          const {
            defaultAssigneeRole, description, managerId, name, sequence,
          } = res;
        ds?.current?.set('defaultAssigneeRole', defaultAssigneeRole);
        ds?.current?.set('description', description);
        ds?.current?.set('sequence', sequence);
        ds?.current?.set('managerId', managerId || undefined);
        ds?.current?.set('name', name);
        setComponent(res);
        });
    }
  }, [componentId, ds]);
  return (
    <Form dataSet={ds} className={styles.component}>
      <TextField name="name" placeholder="请输入模块名称，例如：前端模块" />
      <TextField name="description" />
      <NumberField name="sequence" help="顺序值越大，越靠前。无序列值排在最后，顺序值相同时，按照创建时间倒序排列" />
      <Select name="defaultAssigneeRole">
        {['模块负责人', '无'].map((defaultAssigneeRole) => (
          <Option key={defaultAssigneeRole} value={defaultAssigneeRole}>
            {defaultAssigneeRole}
          </Option>
        ))}
      </Select>
      {
        ds?.current?.get('defaultAssigneeRole') === '模块负责人' && (
        <SelectUser
          name="managerId"
          selected={component?.managerId ? [component.managerId as string] : []}
        />
        )
      }
    </Form>
  );
};

const ObserverCreateComponent = observer(CreateComponent);

const openComponentModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: props.componentId ? '编辑模块' : (
      <C7NFormat
        intlPrefix="agile.setting"
        id="create.component"
      />
    ),
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: (
      <ObserverCreateComponent
        {...props}
      />
    ),
  });
};

export default openComponentModal;
