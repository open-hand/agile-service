import React from 'react';
import './Empty.less';

interface EmptyProps {
  title: React.ReactNode,
  description: React.ReactNode,
  pic: string,
  border?: boolean,
  style?: React.CSSProperties,
  imgStyle?: React.CSSProperties,
}
const Empty: React.FC<EmptyProps> = ({
  style,
  border,
  pic,
  title,
  description,
  imgStyle,
}) => (
  <div
    className="c7nagile-Empty"
    style={style}
  >
    <div
      className="c7nagile-Empty-content"
      style={{
        border: border ? '1px dashed rgba(0, 0, 0, 0.54)' : '',
      }}
    >
      <div className="c7nagile-Empty-imgWrap">
        <img
          src={pic}
          alt=""
          className="c7nagile-Empty-imgWrap-img"
          style={imgStyle}
        />
      </div>
      <div
        className="c7nagile-Empty-textWrap"
      >
        <h1 className="c7nagile-Empty-title">
          {title || ''}
        </h1>
        <div className="c7nagile-Empty-description">
          {description || ''}
        </div>
      </div>
    </div>
  </div>
);
export default Empty;
