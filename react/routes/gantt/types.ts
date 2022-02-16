import { Issue, User } from '@/common/types';

export interface GanttIssue extends Pick<Issue, 'issueId' | 'issueNum' | 'summary' | 'creationDate' | 'issueTypeVO' | 'priorityVO' | 'statusVO'> {
    actualCompletedDate?: string | null
    actualEndTime?: string
    actualStartTime?: string
    assignee?: User | null
    color: null
    completed: boolean
    components: null | any[]
    createUser: User
    environment?: string | null
    epicId?: string
    epicName?: string | null
    estimateTime?: null
    estimatedEndTime?: string
    estimatedStartTime?: string
    featureId?: string | null
    featureName?: string | null
    featureType?: any
    fixVersion?: any
    foundationFieldValue?: any
    influenceVersion?: any
    labels?: any[]
    lastUpdateDate?: string
    mainResponsibleUser?: User
    objectVersionNumber: number
    parentId?: string
    programId?: string | number | null
    projectId?: string | number
    remainingTime: string
    sprint?: { sprintName: string, statusCode: string, sprintId: string }
    sprints?: { sprintName: string, statusCode: string, sprintId: string }[]
    storyPoints?: number
    tags?: any[]
    updateUser?: User
    workTimePercentage: number
    predecessorType?: string
    predecessors?: null | GanttIssue[]
}
