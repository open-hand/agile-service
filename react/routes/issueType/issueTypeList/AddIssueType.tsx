import React, {
  useMemo, useCallback, useState, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Select, DataSet, TextField, TextArea, IconPicker,
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
}

const AddIssueType: React.FC<Props> = ({ modal, typeId, typeTableDataSet }) => {
  const [colour, setColor] = useState<string>('#3F51B5');
  const [pickerDisplay, setPickerDisplay] = useState<boolean>(false);
  const [initType, setInitType] = useState<any>();
  const isOrganization = getIsOrganization();

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
        required: !(initType?.source === 'system'),
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
  }), [checkName, initType?.source, typeId]);

  const handleChangeColor = useCallback((newColor: any) => {
    setColor(newColor.hex);
  }, []);

  const handleClickSwatch = useCallback(() => {
    setPickerDisplay(!pickerDisplay);
  }, [pickerDisplay]);

  const handlePickerHidden = useCallback(() => {
    setPickerDisplay(false);
  }, []);

  const handleSubmit = useCallback(async () => {
    const validate = await addDataSet.validate();
    if (validate) {
      const data = {
        id: typeId,
        colour,
        name: addDataSet.current?.get('name'),
        description: addDataSet.current?.get('description'),
        typeCode: addDataSet.current?.get('typeCode'),
        icon: addDataSet.current?.get('icon'),
      };
      if (!typeId) {
        issueTypeApi[isOrganization ? 'orgCreate' : 'create'](data as ICreate).then(() => {
          Choerodon.prompt('创建成功');
          typeTableDataSet.query();
          modal?.close();
        }).catch(() => {
          Choerodon.prompt('创建失败');
        });
      } else {
        issueTypeApi[isOrganization ? 'orgUpdate' : 'update'](typeId, data as IUpdate).then(() => {
          Choerodon.prompt('编辑成功');
          typeTableDataSet.query(typeTableDataSet.currentPage);
          modal?.close();
        }).catch(() => {
          Choerodon.prompt('编辑失败');
        });
      }
    }
    return false;
  }, [addDataSet, colour, isOrganization, modal, typeId, typeTableDataSet]);

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
      <Form dataSet={addDataSet}>
        <TextField name="name" />
        <TextArea name="description" />
        {
          !(initType?.source === 'system') && (
            <Select name="typeCode" />
          )
        }
        <IconPicker name="icon" />
      </Form>
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
    </div>

  );
};

export default observer(AddIssueType);
