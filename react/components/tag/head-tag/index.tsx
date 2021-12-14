import React, { useCallback } from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import classNames from 'classnames';
import styles from './index.less';

export interface HeadTagProps extends React.HTMLAttributes<HTMLDivElement>{
  src?: string
  name?: React.ReactNode
  avatarStyle?: React.CSSProperties
  avatarClassName?: string
  textStyle?: React.CSSProperties
  textClassName?: string
  tooltip?: boolean | React.ReactNode
  text?: string
  showText?: boolean
  size?: number
  className?: string
  style?: React.CSSProperties
  textShow?: string
}
const HeadTag: React.FC<HeadTagProps> = ({
  size,
  src,
  name,
  avatarStyle,
  avatarClassName,
  text,
  showText,
  textStyle,
  textClassName,
  style,
  className,
  tooltip = true,
  ...otherProps
}) => {
  const avatarSize = size || 20;
  const borderRadius = avatarSize / 3;
  const renderTooltip = useCallback(() => {
    if (typeof tooltip === 'string' || React.isValidElement(tooltip)) {
      return tooltip;
    }
    return text;
  }, [text, tooltip]);

  const content = (
    <div
      className={classNames(styles.head_tag, className, 'c7n-head-tag')}
      style={style}
      {...otherProps}
    >
      <div
        className={classNames(styles.head_tag_avatar, avatarClassName)}
        style={{
          width: avatarSize,
          height: avatarSize,
          lineHeight: `${avatarSize}px`,
          fontSize: `${avatarSize * 13 / 20}px`,
          backgroundImage: src ? `url('${src}')` : undefined,
          borderRadius,
          cursor: tooltip ? 'pointer' : 'auto',
          ...avatarStyle,
        }}
      >
        <span style={{ opacity: !src ? 1 : 0 }}>{name}</span>
      </div>
      {
        text && showText && (
          <span
            className={classNames(styles.head_tag_text, textClassName)}
            style={textStyle}
          >
            {text}
          </span>
        )
      }
    </div>
  );
  return tooltip
    ? (
      <Tooltip title={renderTooltip()} mouseEnterDelay={0.5}>
        {content}
      </Tooltip>
    )
    : content;
};
export default HeadTag;
