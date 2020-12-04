import React, { Component } from 'react';
import {
  CardWidth, CardHeight, CardMargin, CardPadding,
} from '../../Constants';
import './Card.less';

class Card extends Component {
  render() {
    const {
      style, className, saveRef, ...otherProps
    } = this.props;
    return (
      <div
        ref={saveRef}
        style={{
          width: CardWidth,
          height: CardHeight,
          margin: CardMargin,
          padding: CardPadding,
          textAlign: 'left',
          ...style,
        }}
        className={`c7nagile-StoryMap-Card ${className || ''}`}
        {...otherProps}
      />
    );
  }
}

Card.propTypes = {

};

export default Card;
