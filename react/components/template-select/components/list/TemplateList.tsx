import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Tooltip, Popconfirm,
} from 'choerodon-ui';
import classnames from 'classnames';
import { TemplateAction, templateApi } from '@/api';
import { ITableColumnCheckBoxesDataProps } from '@/components/table-column-check-boxes';
import styles from './TemplateList.less';
import openEditTemplate, { IFieldOption, ITemplate } from '../edit/EditTemplate';

interface Props {
  action: TemplateAction
  checkOptions: IFieldOption[]
  templateList: ITemplate[]
  selected: ITemplate | undefined
  setSelected: Function
  templateItemNameCls: string
  onEdit: (template: ITemplate) => void
  onDelete: (id: string) => void
  selectTemplateOk: (codes: string[]) => void,
  transformExportFieldCodes: (data: Array<string>, otherData: ITableColumnCheckBoxesDataProps) => Array<string>
  reverseTransformExportFieldCodes: (data: string[]) => string[]
  defaultInitCodes: string[]
  setHidden: (hidden: boolean) => void
}

const TemplateList: React.FC<Props> = ({
  action, checkOptions, templateList, selected, setSelected, templateItemNameCls, onEdit, onDelete, selectTemplateOk, transformExportFieldCodes, reverseTransformExportFieldCodes, defaultInitCodes, setHidden,
}) => {
  const handleSelect = useCallback((template) => {
    setSelected(template);
    selectTemplateOk(JSON.parse(template.templateJson));
  }, [selectTemplateOk, setSelected]);

  const handleClickEdit = useCallback(({ template }) => {
    setHidden(true);
    setTimeout(() => {
      openEditTemplate({
        template, checkOptions, action, onEdit, transformExportFieldCodes, reverseTransformExportFieldCodes, defaultInitCodes,
      });
    }, 200);
  }, [action, checkOptions, defaultInitCodes, onEdit, reverseTransformExportFieldCodes, setHidden, transformExportFieldCodes]);

  const handleClickDelete = useCallback(async (template) => {
    await templateApi.delete(template.id);
    onDelete(template.id);
  }, [onDelete]);

  return (
    <div className={styles.template_list}>

      {
        templateList && templateList.length ? (
          <>
            {
            templateList.map((template) => (
              <div className={styles.template_item} key={template.id}>
                <span className={classnames(styles.template_item_name, templateItemNameCls)} role="none" onClick={() => handleSelect(template)}>{template.name}</span>
                <div className={styles.template_item_action}>
                  <Tooltip title="修改">
                    <Icon
                      type="edit-o"
                      onClick={() => handleClickEdit({ template })}
                      style={{
                        marginRight: 12,
                      }}
                    />
                  </Tooltip>

                  <Tooltip title="删除">
                    <Popconfirm
                      title="确认要删除该模板吗?"
                      placement="left"
                    // eslint-disable-next-line react/jsx-no-bind
                      onConfirm={() => handleClickDelete(template)}
                      okText="删除"
                      cancelText="取消"
                      okType="danger"
                      overlayStyle={{
                        zIndex: 1051,
                      }}
                    >
                      <Icon type="delete_sweep-o" />
                    </Popconfirm>
                  </Tooltip>
                </div>
              </div>
            ))
          }
          </>
        ) : <div className={styles.hasNoTemplate}>暂无模板</div>
      }
    </div>
  );
};

export default observer(TemplateList);
