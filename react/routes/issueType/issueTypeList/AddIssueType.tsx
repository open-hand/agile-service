import React, {
  useMemo, useCallback, useState, useEffect, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Select, DataSet, TextField, TextArea, IconPicker, Modal,
} from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import { CompactPicker } from 'react-color';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon } from '@choerodon/boot';
import { issueTypeApi, IUpdate, ICreate } from '@/api';
import { getIsOrganization } from '@/utils/common';
import styles from './AddIssueType.less';

interface Props {
  modal: IModalProps,
  typeId?: string
  typeTableDataSet: DataSet
  addRef: React.MutableRefObject<{ addDataSet: DataSet, submit: (fn?: Function) => Promise<boolean> }>
  isOrganization: boolean,
}

const AddIssueType: React.FC<Props> = ({
  modal, typeId, typeTableDataSet, addRef, isOrganization,
}) => {
  const [colour, setColor] = useState<string>('#3F51B5');
  const [pickerDisplay, setPickerDisplay] = useState<boolean>(false);
  const [initType, setInitType] = useState<any>();
  const isSystemType = initType?.source === 'system';

  const checkName = useCallback(async (value) => {
    if (typeId && value === initType?.name) {
      return true;
    }
    const res = isOrganization ? await issueTypeApi.orgCheckName(value, typeId) : await issueTypeApi.checkName(value, typeId);
    if (res) {
      return '名称重复';
    }
    return true;
  }, [initType?.name, isOrganization, typeId]);

  const standardTypeDataSet = useMemo(() => new DataSet({
    data: [
      {
        name: '故事',
        code: 'story',
      },
      {
        name: '任务',
        code: 'task',
      },
      {
        name: '子任务',
        code: 'sub_task',
      },
    ],
    fields: [{
      name: 'name',
      type: 'string' as FieldType,
    }, {
      name: 'code',
      type: 'string' as FieldType,
    }],
  }), []);

  const addDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '名称',
        required: true,
        maxLength: 6,
        validator: checkName,
      },
      {
        name: 'description',
        type: 'string' as FieldType,
        label: '问题类型描述',
      },
      {
        name: 'typeCode',
        type: 'string' as FieldType,
        textField: 'name',
        valueField: 'code',
        label: '标准类型',
        required: !isSystemType,
        disabled: !!typeId,
        options: standardTypeDataSet,
      },
      {
        name: 'icon',
        type: 'string' as FieldType,
        label: '选择图标',
        required: true,
      },
    ],
  }), [checkName, isSystemType, standardTypeDataSet, typeId]);

  const handleChangeColor = useCallback((newColor: any) => {
    setColor(newColor.hex);
  }, []);

  const handleClickSwatch = useCallback(() => {
    if (!isSystemType) {
      setPickerDisplay(!pickerDisplay);
    }
  }, [isSystemType, pickerDisplay]);

  const handlePickerHidden = useCallback(() => {
    setPickerDisplay(false);
  }, []);

  const saveEdit = useCallback((data: IUpdate, callback?: Function) => {
    issueTypeApi[isOrganization ? 'orgUpdate' : 'update'](typeId as string, data).then(() => {
      Choerodon.prompt('编辑成功');
      typeTableDataSet.query(typeTableDataSet.currentPage);
      modal?.close();
      if (callback) {
        callback();
      }
    }).catch(() => {
      Choerodon.prompt('编辑失败');
    });
  }, []);

  const handleSubmit = useCallback(async (callback?: Function) => {
    const validate = await addDataSet.validate();
    if (validate) {
      const name = addDataSet.current?.get('name');
      const description = addDataSet.current?.get('description');
      const typeCode = addDataSet.current?.get('typeCode');
      const icon = addDataSet.current?.get('icon');

      const data = {
        id: typeId,
        colour,
        name,
        description,
        typeCode,
        icon,
      };
      if (!typeId) {
        issueTypeApi[isOrganization ? 'orgCreate' : 'create'](data as ICreate).then(() => {
          Choerodon.prompt('创建成功');
          typeTableDataSet.query();
          modal?.close();
          if (callback) {
            callback();
          }
        }).catch(() => {
          Choerodon.prompt('创建失败');
        });
      } else {
        const isModified = initType?.name !== name || initType?.description != description || initType?.icon !== icon || initType?.colour !== colour;
        if (isModified) {
          if (callback) {
            const confirmModal = Modal.open({
              className: styles.confirm_modal,
              style: {
                width: 520,
              },
              key: Modal.key(),
              title: '尚未保存',
              children: (
                <div>您尚未保存当前页面修改的内容，是否保存？</div>
              ),
              okText: '保存',
              cancelText: '不保存',
              onOk: () => {
                saveEdit(data as IUpdate, callback);
                confirmModal.close();
              },
              onCancel: () => {
                modal?.close();
                callback();
              },
            });
          } else {
            saveEdit(data as IUpdate);
          }
        } else if (callback) {
          callback();
        }
      }
    }
    return false;
  }, [addDataSet, colour, initType?.colour, initType?.description, initType?.icon, initType?.name, isOrganization, modal, saveEdit, typeId, typeTableDataSet]);

  useImperativeHandle(addRef, () => ({
    addDataSet,
    submit: handleSubmit,
  }));

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    if (typeId) {
      issueTypeApi[isOrganization ? 'orgGetType' : 'getType'](typeId).then((res: any) => {
        setInitType(res);
        addDataSet.current?.set('name', res.name);
        addDataSet.current?.set('description', res.description);
        addDataSet.current?.set('icon', res.icon);
        addDataSet.current?.set('typeCode', res.typeCode);
        setColor(res.colour);
      });
    }
  }, [addDataSet, isOrganization, typeId]);
  return (
    <div className={styles.addIssueType}>
      <Form dataSet={addDataSet} disabled={isSystemType}>
        <TextField name="name" />
        <TextArea name="description" />
        {
          !isSystemType && (
            <Select name="typeCode" />
          )
        }
        <IconPicker name="icon" />
      </Form>
      <div className={styles.colorPicker}>
        <div className={`${styles.swatch} ${styles[`swatch_${isSystemType}`]}`} onClick={handleClickSwatch} role="none">
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
    </div>

  );
};

export default observer(AddIssueType);
