import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  Modal, TextField, TextArea, DataSet, Select, Form, CheckBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { observer } from 'mobx-react-lite';
import { IModalProps, IPriority } from '@/common/types';
import { CompactPicker } from 'react-color';
import { priorityApi, UPriority } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import styles from './PriorityModal.less';

interface Props {
  modal?: IModalProps,
  priorityId?: string,
  editingPriority?: IPriority
  name?: string,
  onOk?: () => void
}

const CreatePriority: React.FC<Props> = ({
  modal, onOk, priorityId, editingPriority,
}) => {
  const [colour, setColor] = useState<string>(editingPriority?.colour || '#5365EA');
  const [pickerDisplay, setPickerDisplay] = useState<boolean>(false);
  const checkNameRepeat = useCallback(async (value: string) => {
    if (value && value.trim()) {
      const data: boolean = await priorityApi.checkName(value);
      if (data && value !== editingPriority?.name) {
        return '优先级名称重复';
      }
    }
    return true;
  }, [editingPriority?.name]);

  const ds = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'name',
      label: '名称',
      type: 'string' as FieldType,
      required: true,
      validator: checkNameRepeat,
      maxLength: 10,
    }, {
      name: 'description',
      label: '描述',
      type: 'string' as FieldType,
      maxLength: 45,
    }, {
      name: 'default',
      label: '设置为默认优先级',
      type: 'boolean' as FieldType,
      disabled: !!priorityId && (!!editingPriority?.default || !editingPriority?.enable),
    }],
    data: editingPriority ? [{ name: editingPriority.name, description: editingPriority.description, default: !!editingPriority.default }] : undefined,
  }), [checkNameRepeat, editingPriority, priorityId]);

  const handleSubmit = useCallback(async () => {
    const validate = await ds.validate();
    if (validate) {
      const data = {
        name: ds.current?.get('name'),
        description: ds.current?.get('description'),
        default: !!ds.current?.get('default'),
        colour,
        objectVersionNumber: priorityId ? editingPriority?.objectVersionNumber : undefined,
      };
      priorityId ? await priorityApi.update(priorityId as string, data as UPriority) : await priorityApi.create(data);
      if (onOk) {
        onOk();
      }
      return true;
    }
    return false;
  }, [ds, colour, priorityId, editingPriority?.objectVersionNumber, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  const handlePickerHidden = useCallback(() => {
    setPickerDisplay(false);
  }, []);

  const handleClickSwatch = useCallback(() => {
    setPickerDisplay(!pickerDisplay);
  }, [pickerDisplay]);

  const handleChangeColor = useCallback((newColor: any) => {
    setColor(newColor.hex);
  }, []);

  return (
    <Form dataSet={ds} className={styles.priorityModal}>
      <TextField name="name" placeholder="请输入优先级名称" />
      <TextArea name="description" placeholder="请输入此优先级的详细描述" />
      <div className={styles.colorPicker}>
        <div className={styles.swatch} onClick={handleClickSwatch} role="none">
          <div className={styles.color} style={{ background: colour }} />
        </div>
        {
          pickerDisplay
            ? (
              <div className={styles.popover}>
                <div className={styles.cover} onClick={handlePickerHidden} role="none" />
                <CompactPicker color={colour} onChange={handleChangeColor} />
              </div>
            )
            : null
        }
      </div>
      <CheckBox name="default" />
    </Form>
  );
};

const ObserverCreatePriority = observer(CreatePriority);

const openPriorityModal = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: props.priorityId ? '修改优先级' : '创建优先级',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: (
      <ObserverCreatePriority
        {...props}
      />
    ),
    okText: props.priorityId ? '保存' : '创建',
  });
};

export default openPriorityModal;
