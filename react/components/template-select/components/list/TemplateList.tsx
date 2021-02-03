import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Modal, Tooltip, Popconfirm,
} from 'choerodon-ui';
import useIsProgram from '@/hooks/useIsProgram';
import { TemplateAction, templateApi } from '@/api';
import classnames from 'classnames';
import styles from './TemplateList.less';
import openEditTemplate, { IFieldOption, ITemplate } from '../edit/EditTemplate';

interface Props {
  action: TemplateAction
  checkOptions: IFieldOption[]
  templateList: ITemplate[]
  setSelected: Function
  templateItemNameCls: string
}

const TemplateList: React.FC<Props> = ({
  action, checkOptions, templateList, setSelected, templateItemNameCls,
}) => {
  const { isProgram } = useIsProgram();
  console.log('templateList:');
  console.log(templateList);

  const handleSelect = useCallback((template) => {
    setSelected(template);
  }, [setSelected]);

  const handleClickEdit = useCallback(({ template }) => {
    openEditTemplate({ template, checkOptions, action });
  }, [action, checkOptions]);

  const handleClickDelete = useCallback(async (template) => {
    await templateApi.delete(template.id);
  }, []);

  return (
    <div className={styles.template_list}>
      {
        templateList && templateList.length ? (
          <>
            {
            templateList.map((template) => (
              <div className={styles.template_item}>
                <span className={classnames(styles.template_item_name, templateItemNameCls)} role="none" onClick={() => handleSelect(template)}>{template.name}</span>
                <div className={styles.template_item_action}>
                  <Tooltip title="修改">
                    <Icon
                      type="mode_edit"
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
                    >
                      <Icon type="delete_forever pointer" />
                    </Popconfirm>
                  </Tooltip>
                </div>
              </div>
            ))
          }
          </>
        ) : '暂无模板'
      }
    </div>
  );
};

export default observer(TemplateList);
