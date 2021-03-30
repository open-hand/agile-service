import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  DataSet, Form, Modal, TextField, CheckBox,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon, stores } from '@choerodon/boot';
import { TemplateAction, templateApi } from '@/api';
import { IModalProps } from '@/common/types';
import { getOrganizationId, getProjectId } from '@/utils/common';
import styles from './SaveTemplate.less';

export interface ITemplate {
  id: string
  name: string
  objectVersionNumber: number,
  templateJson: string
}
interface Props {
  modal?: IModalProps,
  onOk: (template: ITemplate) => void
  templateJson: string,
  action: TemplateAction
  template: ITemplate | undefined
}

const { AppState } = stores;

const SaveTemplate: React.FC<Props> = ({
  modal, onOk, templateJson, action, template,
}) => {
  const [checked, setChecked] = useState<'old' | 'new'>(template ? 'old' : 'new');
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
    data: [{
      filterName: template?.name,
    }],
  }), [checkName, template?.name]);

  const handleSubmit = useCallback(async () => {
    if (!await dataSet.current?.validate()) {
      return false;
    }
    const value = dataSet.toData()[0] as any;
    const data = checked === 'old' ? {
      ...template,
      name: value.filterName,
      templateJson,
    } : {
      name: value.filterName,
      templateJson,
      userId: AppState.userInfo.id,
      action,
      type: 'excel',
      projectId: getProjectId(),
      organizationId: getOrganizationId(),
    };
    try {
      let res = {} as ITemplate;
      if (checked === 'old' && template?.id) {
        res = await templateApi.edit(template?.id, data);
      } else {
        res = await templateApi.create(data);
      }
      onOk(res);
      Choerodon.prompt('保存成功');
      return true;
    } catch (error) {
      Choerodon.prompt('保存失败');
      return false;
    }
  }, [dataSet, checked, template, templateJson, action, onOk]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  const handleCheckboxChange = useCallback((value, oldValue) => {
    if (value) {
      setChecked(value);
      if (value === 'old') {
        dataSet?.current?.set('filterName', template?.name);
      } else {
        dataSet?.current?.set('filterName', undefined);
      }
    } else if (oldValue === 'old') {
      setChecked('new');
      dataSet?.current?.set('filterName', undefined);
    } else {
      setChecked('old');
      dataSet?.current?.set('filterName', template?.name);
    }
  }, [dataSet, template?.name]);

  return (
    <Form
      dataSet={dataSet}
      className={styles.form}
    >
      {
        template && (
        <div>
          <CheckBox
            name="option"
            value="old"
            checked={checked === 'old'}
            onChange={handleCheckboxChange}
          >
            更新旧模板
          </CheckBox>
          <CheckBox
            name="option"
            value="new"
            checked={checked === 'new'}
            onChange={handleCheckboxChange}
            style={{
              marginLeft: 30,
            }}
          >
            保存为新模板
          </CheckBox>
        </div>
        )
      }
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
