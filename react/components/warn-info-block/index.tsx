import React, { useState, useEffect, useCallback } from 'react';
import { Icon } from 'choerodon-ui/pro';
import classNames from 'classnames';
import './index.less';

interface IWarnInfoBlockProps {
  className?: string
  style?: React.CSSProperties
  defaultVisible?: boolean
  mode?: 'simple' | 'default'
  visible?: boolean
  content?: React.ReactNode
  predefineContent?: { type: 'requiredNoPermission', props?: { fieldNames: string[] } }
}
const getPredefineContent = (key: string, extraProps: any) => {
  switch (key) {
    case 'requiredNoPermission': {
      const { fieldNames = [] } = extraProps || {};
      return `该问题类型有字段【${fieldNames?.join('、')}】设置为必填并且您无权限查看，请联系项目管理员`;
    }

    default:
      break;
  }
  return undefined;
};
const WarnInfoBlock: React.FC<IWarnInfoBlockProps> = ({
  defaultVisible = true, content, visible: propsVisible, className, style, mode = 'default', predefineContent,
}) => {
  const prefixCls = 'c7n-agile-warn-info-block';
  const [visible, setVisible] = useState(defaultVisible);

  useEffect(() => {
    setVisible(!!propsVisible);
  }, [propsVisible]);
  const renderContent = useCallback(() => {
    if (content) {
      return content;
    } if (predefineContent) {
      return getPredefineContent(predefineContent.type, predefineContent.props) || '';
    }
    return '';
  }, [content, predefineContent]);
  return visible ? (
    <div
      className={classNames(prefixCls, className, {
        [`${prefixCls}-default`]: mode === 'default',
        [`${prefixCls}-simple`]: mode === 'simple',
      })}
      style={style}
    >
      <Icon type="error" className={`${prefixCls}-icon`} />
      <p className={`${prefixCls}-content`}>
        {renderContent()}
      </p>
    </div>
  ) : null;
};

export default WarnInfoBlock;
