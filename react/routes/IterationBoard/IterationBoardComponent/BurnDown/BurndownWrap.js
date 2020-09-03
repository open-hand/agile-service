import React, { Component } from 'react';
import Card from '../Card';
import Burndown from './BurnDown';

function BurnDownWrap({ sprintId, link }) {
  return (
    <Card
      title="燃尽图"
      link={link}
      sprintId={sprintId}
    >
      <Burndown
        sprintId={sprintId}
      />
    </Card>
  );
}

export default BurnDownWrap;
