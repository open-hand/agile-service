import React, { useEffect, useCallback, useMemo } from 'react';
import {
  Modal, Form, TextField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon } from '@choerodon/boot';
import { IModalProps } from '@/common/types';
import { personalFilterApi } from '@/api';

interface Props {
  modal?: IModalProps,
  projectId?: string
  searchVO: any,
  onOk: () => void
}
async function checkName(value: string, projectId?: string) {
  const data: boolean = await personalFilterApi.project(projectId).checkName(value);
  if (data) {
    return '筛选名称重复';
  }

  return true;
}
const SaveFilterModal: React.FC<Props> = (props) => {
  const {
    modal, searchVO, onOk, projectId,
  } = props;
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'filterName',
      label: '筛选名称',
      type: 'string' as FieldType,
      maxLength: 10,
      required: true,
      validator: (v) => checkName(v, projectId),
    }, {
      name: 'default',
      label: '设为默认',
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (!await dataSet.current?.validate()) {
      return false;
    }
    const value = dataSet.toData()[0] as any;
    const data = {
      default: value.default,
      name: value.filterName,
      filterJson: JSON.stringify(searchVO),
      personalFilterSearchVO: searchVO,
    };
    try {
      await personalFilterApi.project(projectId).create(data);
      Choerodon.prompt('保存成功');
      onOk();
      return true;
    } catch (error) {
      Choerodon.prompt('保存失败');
      return false;
    }
  }, [dataSet, onOk, projectId, searchVO]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={dataSet}>
      <TextField
        name="filterName"
        maxLength={10}
        valueChangeAction={'input' as any}
        onInput={(e) => {
          // @ts-ignore
          dataSet.current?.set('filterName', e.target.value);
          dataSet.current?.getField('filterName')?.checkValidity();
        }}
      />
      <CheckBox name="default">设为默认</CheckBox>
    </Form>
  );
};

const openSaveFilterModal = (props: Props) => {
  Modal.open({
    key: 'SaveFilterModal',
    title: '保存筛选',
    children: <SaveFilterModal {...props} />,
  });
};
export default openSaveFilterModal;
