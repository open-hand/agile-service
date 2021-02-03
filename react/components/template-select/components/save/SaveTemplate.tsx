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
import { ITemplate } from '../edit/EditTemplate';
import styles from './SaveTemplate.less';

interface Props {
  modal?: IModalProps,
  onOk: (template: ITemplate) => void
  fieldCodes: string[],
  action: TemplateAction
}

const { AppState } = stores;

const SaveTemplate: React.FC<Props> = ({
  modal, onOk, fieldCodes, action,
}) => {
  const checkName = useCallback(async (value: string) => {
    const data: boolean = await templateApi.checkName(value, action);
    if (data) {
      return '模板名称重复';
    }
    return true;
  }, [action]);

  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'filterName',
      label: '模板名称',
      type: 'string' as FieldType,
      maxLength: 12,
      required: true,
      validator: checkName,
    }],
  }), [checkName]);
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
      const res: ITemplate = await templateApi.create(data);
      onOk(res);
      Choerodon.prompt('保存成功');
      return true;
    } catch (error) {
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
      width: 520,
    },
    className: styles.saveTemplateModal,
    children: <ObserverSaveTemplate {...props} />,
  });
};

export default openSaveTemplate;
