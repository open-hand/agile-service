import React, { useCallback, useEffect, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon, stores } from '@choerodon/boot';
import { TemplateAction, templateApi } from '@/api';
import { IModalProps } from '@/common/types';
import { getOrganizationId, getProjectId } from '@/utils/common';
import styles from './SaveTemplate.less';

interface Props {
  modal?: IModalProps,
  onOk: () => void
  fieldCodes: string[],
  action: TemplateAction
}
async function checkName(value: string, action: TemplateAction) {
  const data: boolean = await templateApi.checkName(value, action);
  if (data) {
    return '筛选名称重复';
  }

  return true;
}

const { AppState } = stores;

const SaveTemplate: React.FC<Props> = ({
  modal, onOk, fieldCodes, action,
}) => {
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'filterName',
      label: '筛选名称',
      type: 'string' as FieldType,
      maxLength: 12,
      required: true,
      validator: checkName,
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (!await dataSet.current?.validate()) {
      return false;
    }
    const value = dataSet.toData()[0] as any;
    const data = {
      name: value.filterName,
      templateJson: fieldCodes,
      userId: AppState.userInfo.id,
      action,
      type: 'excel',
      projectId: getProjectId(),
      organizationId: getOrganizationId(),
    };
    try {
      await templateApi.create(data);
      Choerodon.prompt('保存成功');
      onOk();
      return true;
    } catch (error) {
      console.log(error);
      Choerodon.prompt('保存失败');
      return false;
    }
  }, [action, dataSet, fieldCodes, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={dataSet}>
      <TextField
        name="filterName"
      />
    </Form>
  );
};

const ObserverSaveTemplate = observer(SaveTemplate);

const openSaveTemplate = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '保存模板',
    style: {
      width: 380,
    },
    className: styles.saveTemplateModal,
    children: <ObserverSaveTemplate {...props} />,
  });
};

export default openSaveTemplate;
