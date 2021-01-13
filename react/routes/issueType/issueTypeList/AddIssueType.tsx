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
import { issueTypeApi } from '@/api';
import styles from './AddIssueType.less';

interface Props {
  modal: IModalProps,
  typeId?: string
  typeTableDataSet: DataSet
}

const AddIssueType: React.FC<Props> = ({ modal, typeId, typeTableDataSet }) => {
  const [color, setColor] = useState<string>('#3F51B5');
  const [pickerDisplay, setPickerDisplay] = useState<boolean>(false);
  const [initType, setInitType] = useState<any>();

  const checkName = useCallback(async (value) => {
    if (typeId && value === initType?.name) {
      return true;
    }
    const res = await issueTypeApi.checkName(value);
    if (res) {
      return true;
    }
    return '名称重复';
  }, [initType?.name, typeId]);

  const checkCode = useCallback(async (value: string) => {
    if (typeId && value === initType?.code) {
      return true;
    }
    const startEndWith = /^_|_$/g;
    const contain = /[0-9a-z_]/g;
    const series = /_{2,}/g;

    if (startEndWith.test(value)) {
      return '不能以_开头或结尾';
    }
    if (!contain.test(value)) {
      return '只能包含数字、小写字母、下划线';
    }
    if (series.test(value)) {
      return '不能包含两个_';
    }
    // const res = await issueTypeApi.checkCode(value);
    // if (!res) {
    //   return '编码重复';
    // }
    return true;
  }, [initType?.code, typeId]);

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
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '名称',
        required: true,
        maxLength: 6,
        // validator: checkName,
      },
      {
        name: 'code',
        type: 'string' as FieldType,
        label: '编码',
        required: true,
        maxLength: 32,
        validator: checkCode,
      },
      {
        name: 'description',
        type: 'string' as FieldType,
        label: '问题类型描述',
      },
      {
        name: 'standardType',
        type: 'string' as FieldType,
        textField: 'name',
        valueField: 'code',
        label: '标准类型',
        required: true,
        options: standardTypeDataSet,
      },
      {
        name: 'icon',
        type: 'string' as FieldType,
        label: '选择图标',
      },
    ],
  }), [checkCode, standardTypeDataSet]);

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
    console.log(validate, addDataSet.current?.data, color);
    const data = addDataSet.current?.data;
    if (!typeId) {
      issueTypeApi.create(data).then(() => {
        Choerodon.prompt('创建成功');
        typeTableDataSet.query();
        modal.close();
      }).catch(() => {
        Choerodon.prompt('创建失败');
      });
    } else {
      issueTypeApi.update(typeId, data).then(() => {
        Choerodon.prompt('编辑成功');
        typeTableDataSet.query(typeTableDataSet.currentPage);
        modal.close();
      }).catch(() => {
        Choerodon.prompt('编辑失败');
      });
    }
    return false;
  }, [addDataSet, color, modal, typeId, typeTableDataSet]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    if (typeId) {
      issueTypeApi.getType(typeId).then((res: any) => {
        setInitType(res);
      });
    }
  }, [typeId]);
  return (
    <div className={styles.addIssueType}>
      <Form dataSet={addDataSet}>
        <TextField name="name" />
        <TextField name="code" />
        <TextArea name="description" />
        <Select name="standardType" />
        <IconPicker name="icon" />
      </Form>
      <div className={styles.colorPicker}>
        <div className={styles.swatch} onClick={handleClickSwatch} role="none">
          <div className={styles.color} style={{ background: color }} />
        </div>
        {
          pickerDisplay
            ? (
              <div className={styles.popover}>
                <div className={styles.cover} onClick={handlePickerHidden} role="none" />
                <CompactPicker color={color} onChange={handleChangeColor} />
              </div>
            )
            : null
        }
      </div>
    </div>

  );
};

export default observer(AddIssueType);
