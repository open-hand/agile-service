import React, {
  useState, useEffect, useCallback, memo, ReactElement, useMemo, useRef,
} from 'react';
import {
  Modal, TextField, Form, Button, DataSet,
} from 'choerodon-ui/pro';
import { FuncType } from 'choerodon-ui/pro/lib/button/interface';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import classnames from 'classnames';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { map, uniq } from 'lodash';
import TableColumnCheckBoxes, { ITableColumnCheckBoxesDataProps, useTableColumnCheckBoxes } from '@/components/table-column-check-boxes';
import { IModalProps } from '@/common/types';
import { TemplateAction, templateApi } from '@/api';
import styles from './EditTemplate.less';

interface FormPartProps {
  title: string | ReactElement,
  className?: string,
  children: ReactElement | ReactElement[] | null | Array<ReactElement | null>,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}

const FormPart: React.FC<FormPartProps> = memo((props) => {
  const {
    title, children, btnOnClick,
  } = props;
  const [btnStatus, setBtnStatus] = useState<'ALL' | 'NONE'>();
  function handleClick() {
    let result = true;
    const nextBtnStatus = btnStatus !== 'NONE' ? 'NONE' : 'ALL';
    if (typeof (btnOnClick) === 'function') {
      result = btnOnClick(nextBtnStatus);
    }
    result && setBtnStatus(nextBtnStatus);
  }
  return (
    <div className={classnames(styles.part, props.className)}>
      <div className={styles.part_title}>
        <div className={styles.part_block} />
        <span>{title}</span>
        {!!btnOnClick && (
          <Button
            className={styles.part_btn}
            onClick={handleClick}
            funcType={'flat' as FuncType}
          >
            {btnStatus !== 'NONE' ? '全选' : '全不选'}
          </Button>
        )}
      </div>
      <div className={styles.part_content}>
        {children}
      </div>
    </div>
  );
});

export interface ITemplate {
  id: string
  name: string
  objectVersionNumber: number,
  templateJson: string
}

export interface IFieldOption {
  label: string
  value: string
  system: boolean
  disabled?: boolean,
  defaultChecked?: boolean,
  name?: string
}

interface Props {
  action: TemplateAction
  template: ITemplate
  modal?: IModalProps
  checkOptions: IFieldOption[]
  onEdit: (template: ITemplate) => void
  transformExportFieldCodes: (data: Array<string>, otherData: ITableColumnCheckBoxesDataProps) => Array<string>
  reverseTransformExportFieldCodes: (data: string[]) => string[]
  defaultInitCodes: string[]
}

export const transformTemplateJsonToArr = (templateJson: string) => {
  const templateParse = JSON.parse(templateJson);
  return Array.isArray(templateParse) ? templateParse : [...(templateParse.systemFields || []), ...(templateParse.customFields || [])];
};

const EditTemplate: React.FC<Props> = ({
  modal, template, checkOptions, action, onEdit, transformExportFieldCodes, reverseTransformExportFieldCodes, defaultInitCodes,
}) => {
  const [, setUpdateCount] = useState(0);
  const templateFieldsRef = useRef();

  useEffect(() => {
    Object.assign(templateFieldsRef, {
      current: reverseTransformExportFieldCodes(transformTemplateJsonToArr(template.templateJson)),
    });
    setUpdateCount((count) => count + 1);
  }, [reverseTransformExportFieldCodes, template.templateJson]);

  const checkName = useCallback(async (value, name, record) => {
    if (value === template.name) {
      return true;
    }
    const res = await templateApi.checkName(value, action);
    if (!res) {
      return true;
    }
    return '模板名称重复';
  }, [action, template.name]);

  const templateDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'templateName',
      label: '模板名称',
      type: 'string' as FieldType,
      maxLength: 12,
      required: true,
      validator: checkName,
    }],
  }), [checkName]);

  useEffect(() => {
    templateDataSet?.current?.set('templateName', template.name);
  }, [template.name, templateDataSet]);

  // 选择字段框配置 数据
  const [checkBoxDataProps, checkBoxComponentProps] = useTableColumnCheckBoxes({
    name: 'templateCodes',
    options: checkOptions,
    defaultValue: templateFieldsRef?.current,
  });

  const handleOk = useCallback(async () => {
    const validate = await templateDataSet.validate();
    if (!validate) {
      return false;
    }
    const templateName = templateDataSet.current?.get('templateName');

    const fieldCodesArr = uniq(transformExportFieldCodes(checkBoxDataProps.checkedOptions, checkBoxDataProps));

    const reverseFieldCodes = reverseTransformExportFieldCodes(fieldCodesArr).filter((code: string) => map(checkOptions, 'value').includes(code));

    const newFieldCodesArr = transformExportFieldCodes(reverseFieldCodes, checkBoxDataProps);

    const fieldCodesObj: {
      systemFields: string[]
      customFields: string[]
    } = { systemFields: [], customFields: [] };

    fieldCodesObj.systemFields = newFieldCodesArr.filter((code) => checkOptions.find((item: any) => item.value === code && item.system));
    fieldCodesObj.customFields = newFieldCodesArr.filter((code) => checkOptions.find((item:any) => item.value === code && !item.system));

    if (checkBoxDataProps.checkedOptions.length === 0) {
      Choerodon.prompt('请至少选择一个字段');
      return false;
    }
    const data = {
      ...template,
      name: templateName,
      templateJson: action === 'agile_import_issue' || action === 'program_import_feature' || action === 'program_import_backlog' || action === 'agile_import_backlog' ? JSON.stringify(fieldCodesObj) : JSON.stringify(newFieldCodesArr),
    };
    const newTemplate: ITemplate = await templateApi.edit(template.id, data);
    onEdit(newTemplate);
    modal?.close();
    return false;
  }, [action, checkBoxDataProps, checkOptions, modal, onEdit, reverseTransformExportFieldCodes, template, templateDataSet, transformExportFieldCodes]);

  useEffect(() => {
  modal?.handleOk(handleOk);
  }, [handleOk, modal]);

  const handleChangeFieldStatus = (status: 'ALL' | 'NONE') => {
    if (status !== 'ALL') {
      checkBoxDataProps.actions.checkAll();
    } else {
      checkBoxDataProps.actions.unCheckAll();
    }
    return true;
  };

  return (
    <div className={styles.template_edit}>
      <FormPart title="修改名称" className={styles.template_edit_name}>
        <div className={styles.template_edit_name}>
          <Form dataSet={templateDataSet}>
            <TextField name="templateName" />
          </Form>
        </div>
      </FormPart>
      <FormPart title="修改模板选择字段" btnOnClick={handleChangeFieldStatus}>
        <div className={styles.template_edit_fields}>
          <TableColumnCheckBoxes {...checkBoxComponentProps} />
        </div>
      </FormPart>
    </div>
  );
};

const ObserverEditTemplate = observer(EditTemplate);

const openEditTemplate = (props: Props) => {
  Modal.open({
    drawer: true,
    key: Modal.key(),
    title: '修改模板',
    style: {
      width: 740,
    },
    className: styles.editTemplateModal,
    children: <ObserverEditTemplate {...props} />,
  });
};

export default openEditTemplate;
