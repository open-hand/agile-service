export interface User {
  email: string
  enabled?: boolean
  id: number
  imageUrl: string | null
  ldap: boolean
  loginName: string
  realName: string
}

export interface Issue {
  issueId: number
  summary: string
}

export interface PI {
  code: string
  name: string
}
