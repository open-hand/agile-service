import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Icon, Tooltip, Popconfirm,
} from 'choerodon-ui';
import { templateApi } from '@/api';
import classnames from 'classnames';
import styles from './TemplateList.less';
import { ITemplate } from '../save/SaveTemplate';

interface Props {
  templateList: ITemplate[]
  setSelected: Function
  templateItemNameCls: string
  onDelete: (id: string) => void
  selectTemplateOk: (codes: string[]) => void,
}

const TemplateList: React.FC<Props> = ({
  templateList, setSelected, templateItemNameCls, onDelete, selectTemplateOk,
}) => {
  const handleSelect = useCallback((template) => {
    setSelected(template);
    selectTemplateOk(JSON.parse(template.templateJson));
  }, [selectTemplateOk, setSelected]);

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
        ) : <div className={styles.hasNoTemplate}>暂无模板</div>
      }
    </div>
  );
};

export default observer(TemplateList);
